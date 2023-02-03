local 
exception heading_level_exceeded
exception strong_error
exception em_error
(* fun start : (char List , string List) => unit ;  *)
fun rev_helper (nil , l2) = l2 | rev_helper (h :: tl , l2) = rev_helper( tl , h :: l2) ; 
fun head(nil) = "" | head(x :: tl) = x 

fun push(x , l) = x:: l ; 
fun pop([]) = nil | pop(head :: tl) = tl   
fun rev(l) = rev_helper(l , nil) ; 
fun append(nil,l2) = l2 | 
    append(h :: l , l2) = h :: append(l,l2) ; 

val new_input = TextIO.openIn "./input.txt" ; 
val new_output = TextIO.openAppend "./output.txt" ;
fun write s = TextIO.output(new_output, s) ; 


fun start(temp,stack) = 
    let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (if head(stack) = "<p>" then let val o2 = write(implode(rev(temp))) 
                                                            val o1 = write("</p>\n") in () end 
                            else ())  
        else  
            let val char = Option.getOpt(option,#"%") 
            in start_helper(temp,stack,char) 
            end 
    end   

and start_helper(temp,stack, char) = 
 
            case char of 
                #"#" => (let val h = head(stack) in 
                            if (h = "<p>") then 
                                                  let val o1 = write(implode(rev(temp))) ;   
                                                  val o1 = write("</p>\n") 
                                                  in heading([], pop(stack),1,false) 
                                                  end
                            else heading([], stack, 1,false) 
                        end  
                        )  
                | #"*" => (let val h = head(stack) in 
                                if (h <> "<p>") then let val o2 = write("<p>") ; val o1 = write(implode(rev(temp))) 
                                                    in em_opening_check([],push("<p>",stack),[]) 
                                                    end 
                                else let val o1 = write(implode(rev(temp))) ; 
                                     in em_opening_check([],stack,[])
                                     end 
                            end)
                            
                |    #"\n" => (let val h = List.hd(temp) in 
                          if h = #"\n" then 
                                              if head(stack) = "<p>" then  
                                                                            let val o2 = write(implode(rev(temp))) ; 
                                                                            val o1 = write("</p>\n")
                                                                            in newline([],pop(stack))
                                                                            end 
                                              else newline(temp,pop(stack))  
                          else  
                             start(char :: temp,stack) 
                          end 
                          handle Empty => start(char :: temp, stack)) 
           |    _ => (let val h = head(stack)
                      in  if(h <> "<p>" orelse h = "")then 
                                                                let val o1 = write("<p>") 
                                                                in start(char::temp,push("<p>",stack))
                                                                end 
                          else start(char :: temp,stack)
                      end)  
              
(* written denotes whether openign tag has been written *)
and heading(temp, stack, level, written) =  let 
                                    val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
                                in 
                                    if (not test) then (if written = true then 
                                                    let
                                                    
                                                    val o2 = write(implode(rev(temp))) 
                                                    val o1 = write(getheading(2,level)) 
                                                in () end 
                                                else let 
                                                        val o3 = write(getheading(1,level)) 
                                                         val o2 = write(implode(rev(temp))) 
                                                        val o1 = write(getheading(2,level))
                                                in () end 
                                                ) 
                                    else let val char = Option.getOpt(option,#"%")
                                    in 
                                         case char of 
                                         #"#" => (if level > 5 then raise heading_level_exceeded
                                                  else heading(temp, stack, level + 1,false)
                                         ) 
                                        | #"*" => ( let val o1 =write(implode(rev(temp))) 
                                                    in em_opening_check([],stack,[]) 
                                                    end 
                                                   )
                                        | #"\n" => (let
                                                    
                                                    val o2 = write(implode(rev(temp))) 
                                                    val o1 = write(getheading(2,level)) 
                                                in start([], pop(stack))
                                                end ) 
                                        | _ => (if written = false then 
                                                    let 
                                                    val h = getheading(1,level)
                                                    val o3 = write(h) 
                                                    in heading(char :: temp, push(h,stack), level, true)
                                                    end 
                                                else heading(char :: temp, stack, level,true ))   
                                    end 
                                end 

and getheading(counter, level) = if counter = 2 then case level of 
                        1 => "</h1>\n" |    2 => "</h2>\n" |    3 => "</h3>\n" |    4 => "</h4>\n" |    5 => "</h5>\n" |    6 => "</h6>\n" | _ => "</h6>\n"  
                                else case level of 
                        1 => "<h1>" |    2 => "<h2>" |    3 => "<h3>" |    4 => "<h4>" |    5 => "<h5>" |    6 => "<h6>"  | _ => "<h6>"

and newline(temp, stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (if head(stack) = "<p>" then let val o2  = write(implode(rev(temp))) 
                                                            val o1 = write("</p>\n") in () end 
                            else ())  
        else let val char = Option.getOpt(option,#"%")  
             in case char of 
                #"\n" => newline(temp,stack)   
            |    #"#" => (let val h = head(stack) in 
                            if (h = "<p>") then 
                                                  let val o1 = write(implode(rev(temp))) ;   
                                                  val o1 = write("</p>\n") 
                                                  in heading([], pop(stack),1,false) 
                                                  end
                            else heading([], stack, 1,false) 
                        end  
                        )  
            | #"*" => (let val h = head(stack) in 
                                if (h <> "<p>") then let val o2 = write("<p>") ; val o1 = write(implode(rev(temp))) 
                                                    in em_opening_check([],push("<p>",stack),[]) 
                                                    end 
                                else let val o1 = write(implode(rev(temp))) ; 
                                     in em_opening_check([],stack,[])
                                     end 
                            end)
           |    _ => (let val h = head(stack)
                      in  if(h <> "<p>" orelse h = "")then 
                                                                let val o1 = write("<p>") 
                                                                in start(char::temp,push("<p>",stack))
                                                                end 
                          else start(char :: temp,stack)
                      end)  
             end 
    end 
and strong_opening_check(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in 
              case char of 
              #"*" => raise strong_error 
            | _ => (let val o1 = write("<strong>") in strong_opening(temp, push("<strong>",stack), char::actual) end ) 
            end 
    end 
and strong_opening(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => strong_closing_check(temp,stack,actual) 
            | _ => strong_opening(temp, stack, char :: actual) 
             end   
    end
and strong_closing_check(temp,stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => strong_closing(temp,stack,actual) 
            | _ => em_inside_strong(temp,stack,char::actual)  
            end  
    end

(* here first char of actual is actuaally part of em text, thus handled accordingly *)
and em_inside_strong(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error)  
        else  let val char = Option.getOpt(option,#"%")  in 
              case char of 
              #"*" => (let val o2 = write("<em>") ; val o1 = write(str(hd(actual))) ;val o3 = write("</em>") in em_inside_strong_handler(temp,stack,[]) end )  
             | _ => (let val o2 = write(implode(rev(tl(actual)))) ; val o1 = write("<em>") ;  in em_opening(temp, push("<em>",stack), char::hd(actual)::nil) end )
            end 
    end  
(* to handle case ** <text> *<char>* <text>** *)
and em_inside_strong_handler(temp,stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error) 
        else let val char = Option.getOpt(option,#"%")  in 
            case char of 
              #"*" => raise strong_error 
            | _ => strong_opening(temp, stack, char::actual) 
            end 
    end 



(* modify acc to em *)
and strong_closing(temp, stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (let val callee = head(tl(stack)) val o2 = write(implode(rev(actual))) ; val o3 = write("</em>") 
                    in case callee of 
                      "<h1>" => heading([],pop(stack),1,true) | "<h2>" => heading([],pop(stack),2,true) |"<h3>" => heading([],pop(stack),3,true) 
                       |"<h4>" => heading([],pop(stack),4,true) |"<h5>" => heading([],pop(stack),5,true) |"<h6>" => heading([],pop(stack),6,true)  
                    | "<em>" =>  em_opening(temp,pop(stack),nil)
                    | _ => start(temp,stack) end)

        else  let val char = Option.getOpt(option,#"%") 
              in  case char of
              #"*" => raise strong_error 
            | _ => (let val callee = head(tl(stack)) ; val o2 = write(implode(rev(actual))) ; val o3 = write("</strong>") 
                    in case callee of 
                       "<h1>" => heading(char::nil,pop(stack),1,true) | "<h2>" => heading(char::nil,pop(stack),2,true) |"<h3>" => heading(char::nil,pop(stack),3,true) 
                       |"<h4>" => heading(char::nil,pop(stack),4,true) |"<h5>" => heading(char::nil,pop(stack),5,true) |"<h6>" => heading(char::nil,pop(stack),6,true)  
                    | "<em>" => em_opening(temp,pop(stack),char :: nil) 
                    | _ => start_helper(temp, pop(stack),char) end )  
            end  
    end
and em_opening_check(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise em_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in 
              case char of 
              #"*" => strong_opening_check(temp,stack,actual)  
            | _ => (let val o1 = write("<em>") in em_opening(temp, push("<em>",stack), char::actual) end )
            end 
    end 
and em_opening(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise em_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => em_closing_check(temp,stack,actual) 
            | _ => em_opening(temp, stack, char :: actual) 
             end   
    end
(*    *<text>**   *)
and strong_inside_emphasis(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then raise em_error 
        else        let val char = Option.getOpt(option,#"%")  in 
                    case char of 
                    #"*" => raise em_error  
                    | _ => (let val o1 = write(implode(rev(temp))) ; val o2 = write("<strong>") ; in
                            strong_opening([], push("<strong>",stack), char :: actual) 
                            end ) 
                    end 
    end  




and em_closing_check(temp,stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then  (let val callee = head(tl(stack)) val o2 = write(implode(rev(actual))) ; val o3 = write("</em>") 
                    in case callee of 
                      "<h1>" => heading([],pop(stack),1,true) | "<h2>" => heading([],pop(stack),2,true) |"<h3>" => heading([],pop(stack),3,true) 
                       |"<h4>" => heading([],pop(stack),4,true) |"<h5>" => heading([],pop(stack),5,true) |"<h6>" => heading([],pop(stack),6,true)  
                    | "<strong>" => strong_opening(temp,pop(stack),nil)   
                    | _ => start(temp,stack) end)

        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => (let val o1 = write(implode(rev(actual))) ; in strong_inside_emphasis(temp, stack, []) end )    
            | _ =>  (let val callee = head(tl(stack)) val o2 = write(implode(rev(actual))) ; val o3 = write("</em>") 
                    in case callee of 
                      "<h1>" => heading(char::nil,pop(stack),1,true) | "<h2>" => heading(char::nil,pop(stack),2,true) |"<h3>" => heading(char::nil,pop(stack),3,true) 
                       |"<h4>" => heading(char::nil,pop(stack),4,true) |"<h5>" => heading(char::nil,pop(stack),5,true) |"<h6>" => heading(char::nil,pop(stack),6,true)  
                    | "<strong>" => strong_opening(temp,pop(stack),char::nil)   
                    | _ => start_helper(temp, pop(stack),char) end ) 
            end
    end







in  
fun convert() = start([],[]) ; 
end 

