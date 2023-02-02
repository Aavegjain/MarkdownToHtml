local 
exception heading_level_exceeded
exception strong_error
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
                                                  in heading([], pop(stack),1) 
                                                  end
                            else heading([], stack, 1) 
                        end  
                        )  
                            
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
                          end )
           |    _ => (let val h = head(stack)
                      in  if(h <> "<p>" orelse h = "")then 
                                                                let val o1 = write("<p>") 
                                                                in start(char::temp,push("<p>",stack))
                                                                end 
                          else start(char :: temp,stack)
                      end)  
              

and heading(temp, stack, level) =  let 
                                    val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
                                in 
                                    if (not test) then (let
                                                    val o3 = write(getheading(1,level)) 
                                                    val o2 = write(implode(rev(temp))) 
                                                    val o1 = write(getheading(2,level)) 
                                                in ()
                                                end ) 
                                    else let val char = Option.getOpt(option,#"%")
                                    in 
                                         case char of 
                                         #"#" => (if level > 5 then raise heading_level_exceeded
                                                  else heading(temp, stack, level + 1)
                                         ) 
                                        | #"\n" => (let
                                                    val o3 = write(getheading(1,level)) 
                                                    val o2 = write(implode(rev(temp))) 
                                                    val o1 = write(getheading(2,level)) 
                                                in start([], stack)
                                                end ) 
                                        | _ => heading(char :: temp, stack, level)  
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
                                                  in heading([], pop(stack),1) 
                                                  end
                            else heading([], stack, 1) 
                        end  
                        )  
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
            | _ => strong_opening(temp, stack, char::actual)
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
            | _ => raise strong_error   
            end  
    end
and strong_closing(temp, stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then ()  
        else  let val char = Option.getOpt(option,#"%") 
              in  case char of
              #"*" => raise strong_error 
            | _ => (let val o1 = write("<strong>"); val o2 = write(implode(rev(actual))) ; val o3 = write("</strong>") 
                    in start_helper(temp, stack,char) end )  
            end  
    end





in  
fun convert() = start([],[]) ; 
end 

