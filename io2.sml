local 
exception heading_level_exceeded
exception strong_error
exception em_error
exception underline_error
exception heading_in_asterisk_error
exception heading_in_underline_error
exception asterisk_inside_underline_error
exception list_error 
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
               
                 #"*" => (let val h = head(stack) in 
                                if (h <> "<p>") then let val o2 = write("<p>") ; val o1 = write(implode(rev(temp))) 
                                                    in em_opening_check([],push("<p>",stack),[]) 
                                                    end 
                                else let val o1 = write(implode(rev(temp))) ; 
                                     in em_opening_check([],stack,[])
                                     end 
                            end)
                | #"_" => (let val h = head(stack) in 
                                if (h <> "<p>") then let val o2 = write("<p>") ; val o1 = write(implode(rev(temp))) ; val o3 = write("<u>"); 
                                                    in underline([],"<u>" :: "<p>" :: stack,[]) 
                                                    end 
                                else let val o1 = write(implode(rev(temp))) ; val o3 = write("<u>");
                                     in underline([],"<u>" ::stack,[])
                                     end 
                            end)
                
                |    #"\n" => newline_starting_tags(char::temp,stack,false) 
                
                (* (let val h = List.hd(temp) in 
                          if h = #"\n" then 
                                              if head(stack) = "<p>" then  
                                                                            let val o2 = write(implode(rev(temp))) ; 
                                                                            val o1 = write("</p>\n")
                                                                            in newline([],pop(stack))
                                                                            end 
                                              else newline(temp,pop(stack))  
                          else  
                             start(char :: temp,stack)end 
                          handle Empty => start(char :: temp, stack))  *)
           |    _ => (let val h = head(stack)
                      in  if(h <> "<p>" orelse h = "")then 
                                                                let val o1 = write("<p>") 
                                                                in start(char::temp,push("<p>",stack))
                                                                end 
                          else start(char :: temp,stack)
                      end)  
              
(* occured denotes that a blank line has occured *)
and newline_starting_tags(temp,stack,occured) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (if head(stack) = "<p>" then let val o2 = write(implode(rev(temp))) 
                                                            val o1 = write("</p>\n") in () end 
                            else ())  
        else  
            let val char = Option.getOpt(option,#"%") 
            in case char of 
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
                | #"_" => (let val h = head(stack) in 
                                if (h <> "<p>") then let val o2 = write("<p>") ; val o1 = write(implode(rev(temp))) ; val o3 = write("<u>"); 
                                                    in underline([],"<u>" :: "<p>" :: stack,[]) 
                                                    end 
                                else let val o1 = write(implode(rev(temp))) ; val o3 = write("<u>");
                                     in underline([],"<u>" ::stack,[])
                                     end 
                            end)
                | #"-" => (let val h = head(stack) in 
                                if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   
                                                    in unordered_list_check([],pop(stack)) 
                                                    end 
                                else unordered_list_check([],stack) 
                            end)
                | #">" => blockquote_control([],push("<blockquote>",stack),1,0)
                | #"1" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"2" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"3" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"4" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"5" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"6" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"7" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"8" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"9" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end else ordered_list_check_dot(char::[],stack) end)
                | #"0" => (let val h = head(stack) in if (h = "<p>") then let val o1 = write(implode(rev(temp))) ; val o2=write("</p>");   in ordered_list_check_dot(char::[],pop(stack)) end  else ordered_list_check_dot(char::[],stack) end)


                
                
                
                
                
                
                
                
                | #"\n" => if (occured = true) then newline_starting_tags(temp,stack,occured)  
                            else newline_starting_tags(temp,stack,true) 
                | _ => if (occured = true) then (if head(stack) = "<p>" then  
                                                                            let val o2 = write(implode(rev(temp))) ; 
                                                                            val o1 = write("</p>\n")
                                                                            in newline_starting_tags(char::nil,pop(stack),false)
                                                                            end 
                                                    else 
                                                        let val o3 = write("<p>") 
                                                            val o2 = write(implode(rev(temp))) ; 
                                                            val o1 = write("</p>\n")
                                                            in newline_starting_tags(char::nil,push("<p>",stack),false)
                                                            end )
                        else start(char::temp,stack) 
            end 
    end    


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
                                                    end)
                                        | #"_" => (if written=true then let val o1 = write(implode(rev(temp))) ; val o2 = write("<u>") ; 
                                                            in underline([],"<u>" :: stack,[]) end 
                                                    else let val o3 = write(getheading(1,level)) ;val o1 = write(implode(rev(temp))) ; val o2 = write("<u>") ; 
                                                            in underline([],"<u>" :: getheading(1,level)  :: stack,[]) end )
                                        | #"\n" => (let
                                                    
                                                    val o2 = write(implode(rev(temp))) 
                                                    val o1 = write(getheading(2,level)) 
                                                in start_helper([], pop(stack),#"\n") 
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
            (* | #"_" => (let val o1 = write("<strong>") ; val o2 = write("<u>") ; in underline([],"<u>" :: "<strong>" :: stack,[]) end ) *)
            | #"_" => raise underline_error
            | #"#" => raise heading_in_asterisk_error
            
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
            | #"#" => raise heading_in_asterisk_error
             (* | #"_" => (if head(stack) = "<u>" then raise underline_error 
                        else   let val o1 = write(implode(rev(actual))) ; val o2 = write("<u>") ; in underline([],"<u>" :: stack,[]) end )  *)
            | #"_" => raise underline_error
            | _ => strong_opening(temp, stack, char :: actual) 
             end   
    end
(* **text*_ *)
and strong_closing_check(temp,stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => strong_closing(temp,stack,actual) | #"#" => raise heading_in_asterisk_error
            (* | #"_" => (let val o2 = write(implode(rev(tl(actual)))); val o1 = write("<em>") ; val o3 = write("<u>") 
                        in underline([],"<u>" :: "<em>" :: stack,[]) end )  *) 
            | #"_" => raise underline_error
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
              #"*" => (let val o4 = write(implode(rev(tl(actual)))) ; val o2 = write("<em>") ; val o1 = write(str(hd(actual))) ;val o3 = write("</em>") in em_inside_strong_handler(temp,stack,[]) end )
              | #"#" => raise heading_in_asterisk_error  
            (* | #"_" => (let val o2 = write(implode(rev(tl(actual)))) ; val o1 = write("<em>"^str(hd(actual))) ; val o3 = write("<u>") ; in underline([],"<u>" :: "<em>" :: stack,[]) end) *)
            | #"_" => raise underline_error
             | _ => (let val o2 = write(implode(rev(tl(actual)))) ; val o1 = write("<em>") ;  in em_opening(temp, push("<em>",stack), char::hd(actual)::nil) end )
            end 
    end  
(* to handle case ** <text> *<char>* <text>**   *)
and em_inside_strong_handler(temp,stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise strong_error) 
        else let val char = Option.getOpt(option,#"%")  in 
            case char of 
              #"*" => raise strong_error | #"#" => raise heading_in_asterisk_error 
            (* | #"_" => (let val o1=write("<u>") ; in underline([],"<u>" :: stack,[]) end ) *)
            | #"_" => raise underline_error
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
                    | "<ol>" => ordered_list([],pop(stack)) 
                    | "<u>" => underline([],pop(stack),nil) 
                    | "<ul>" => unordered_list_item([],pop(stack))
                    | _ => start(temp,stack) end)

        else  let val char = Option.getOpt(option,#"%") 
              in  case char of
              #"*" => raise strong_error | #"#" => raise heading_in_asterisk_error
            (* | #"_" =>(let  val o2 = write(implode(rev(actual))) ; val o3 = write("</strong>") ; val o1 = "<u>"; 
                    in underline([],push("<u>",pop(stack)) , []) end )  *)

            | #"\n" => (let val callee = head(tl(stack)) ; val o2 = write(implode(rev(actual))) ; val o3 = write("</strong>") 
                    in case callee of 
                       "<h1>" => let val o1 = write(getheading(2,1)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |  "<h2>" => let val o1 = write(getheading(2,2)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h3>" => let val o1 = write(getheading(2,3)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h4>" => let val o1 = write(getheading(2,4)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h5>" => let val o1 = write(getheading(2,5)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h6>" => let val o1 = write(getheading(2,6)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<ul>" => unordered_list_item_check([],pop(stack)) 
                    |  "<ol>" => ordered_list_item_check([],pop(stack)) 

                    | "<em>" => em_opening(temp,pop(stack),char :: nil) 
                    | "<u>" => (if char = #"_" then underline([],pop(stack),#" "::nil) 
                                else underline([],pop(stack),char::nil)) 
                    | _ => start_helper(temp, pop(stack),char) end )
            | _ => (let val callee = head(tl(stack)) ; val o2 = write(implode(rev(actual))) ; val o3 = write("</strong>") 
                    in case callee of 
                       "<h1>" => heading(char::nil,pop(stack),1,true) | "<h2>" => heading(char::nil,pop(stack),2,true) |"<h3>" => heading(char::nil,pop(stack),3,true) 
                       |"<h4>" => heading(char::nil,pop(stack),4,true) |"<h5>" => heading(char::nil,pop(stack),5,true) |"<h6>" => heading(char::nil,pop(stack),6,true)  
                    | "<em>" => em_opening(temp,pop(stack),char :: nil) 
                    | "<u>" => (if char = #"_" then underline([],pop(stack),#" "::nil) 
                                else underline([],pop(stack),char::nil)) 
                    | "<ul>" => unordered_list_item(char::[],pop(stack))
                    | "<ol>" => ordered_list(char::[],pop(stack)) 

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
              #"*" => strong_opening_check(temp,stack,actual) | #"#" => raise heading_in_asterisk_error
            (* | #"_" => (let val o1 = write("<em>") ; val o2 = write("<u>") ; in underline([],"<u>" :: "<em>" :: stack,[]) end )    *)
            | #"_" => raise underline_error
            | _ => (let val o1 = write("<em>") in em_opening(temp, push("<em>",stack), char::actual) end )
            end 
    end 
and em_opening(temp, stack, actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (raise em_error)  
        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => em_closing_check(temp,stack,actual) | #"#" => raise heading_in_asterisk_error
            (* | #"_" => (let val o1 = write(implode(rev(actual))) ; val o2 = write("<u>") ; in underline([],"<u>" :: stack,[]) end ) *)
            | #"_" => raise underline_error
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
                    #"*" => raise em_error | #"#" => raise heading_in_asterisk_error 
                (* |   #"_" => (let val o1 = write(implode(rev(temp))) ; val o2 = write("<strong>") ; val o3 = write("<u>"); in *)
                             (* underline([],"<u>" :: "<strong>" :: stack , []) end )    *)
                    | #"_" => raise underline_error
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
                    | "<u>" => underline([],pop(stack),[])
                    | "<ul>" => unordered_list_item([],pop(stack)) 
                    | "<ol>" => ordered_list([],pop(stack))
                    | "<strong>" => strong_opening(temp,pop(stack),nil)   
                    | _ => start(temp,pop(stack)) end)

        else  let val char = Option.getOpt(option,#"%") 
              in case char of 
              #"*" => (let val o1 = write(implode(rev(actual))) ; in strong_inside_emphasis(temp, stack, []) end )    
            | #"#" => raise heading_in_asterisk_error
             (* | #"_" =>(let  val o2 = write(implode(rev(actual))) ; val o3 = write("</em>") ; val o1 = "<u>"; 
                    in underline([],push("<u>",pop(stack)) , []) end )  *)
            
            | #"\n" => (let val callee = head(tl(stack)) ; val o2 = write(implode(rev(actual))) ; val o3 = write("</em>") 
                    in case callee of 
                       "<h1>" => let val o1 = write(getheading(2,1)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |  "<h2>" => let val o1 = write(getheading(2,2)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h3>" => let val o1 = write(getheading(2,3)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h4>" => let val o1 = write(getheading(2,4)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h5>" => let val o1 = write(getheading(2,5)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    |   "<h6>" => let val o1 = write(getheading(2,6)^"\n") in start_helper(nil,pop(pop(stack)),char) end 
                    | "<ul>" => unordered_list_item_check([],pop(stack)) 
                    | "<ol>" => ordered_list_item_check([],pop(stack))

                    | "<strong>" => em_opening(temp,pop(stack),char :: nil) 
                    | "<u>" => (if char = #"_" then underline([],pop(stack),#" "::nil) 
                                else underline([],pop(stack),char::nil)) 
                    | _ => start_helper(temp, pop(stack),char) end )
            | _ =>  (let val callee = head(tl(stack)) val o2 = write(implode(rev(actual))) ; val o3 = write("</em>") 
                    in case callee of 
                      "<h1>" => heading(char::nil,pop(stack),1,true) | "<h2>" => heading(char::nil,pop(stack),2,true) |"<h3>" => heading(char::nil,pop(stack),3,true) 
                       |"<h4>" => heading(char::nil,pop(stack),4,true) |"<h5>" => heading(char::nil,pop(stack),5,true) |"<h6>" => heading(char::nil,pop(stack),6,true)  
                    | "<strong>" => strong_opening(temp,pop(stack),char::nil)   
                    | "<u>" => (if char = #"_" then underline([],pop(stack),#" "::nil) 
                                else underline([],pop(stack),char::nil)) 
                    | "<ol>" => ordered_list(char::[],pop(stack))

                    | "<ul>" => unordered_list_item(char::[],pop(stack)) 
                    | _ => start_helper(temp, pop(stack),char) end ) 
            end
    end

and underline(temp,stack,actual) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then raise underline_error 
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #"_" => (if (length actual = 0) then raise underline_error 
                     else underline_closing(temp, stack, #" "::actual)) 
        |   #" " => raise underline_error | #"#" => raise heading_in_underline_error
        |   #"*" => (let val o1 = write(implode(rev(actual))); in em_opening_check([],stack,[]) end ) 
        
        |   _ => underline(temp, stack, char :: actual) 
            end 
    end 

and underline_closing(temp,stack, actual) =  let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (let val o1 = write(implode(rev(tl(actual)))) ; val o2 = write("</u>"); val callee = head(tl(stack));  
                     in case callee of 
                      "<h1>" => heading(nil,pop(stack),1,true) | "<h2>" => heading(nil,pop(stack),2,true) |"<h3>" => heading(nil,pop(stack),3,true) 
                       |"<h4>" => heading(nil,pop(stack),4,true) |"<h5>" => heading(nil,pop(stack),5,true) |"<h6>" => heading(nil,pop(stack),6,true)  
                    | "<strong>" => strong_opening(temp,pop(stack),nil) 
                    | "<em>" => em_opening(temp,pop(stack),nil)     
                    | "<ul>" => unordered_list_item([],pop(stack)) 
                    | "<ol>" => ordered_list([],pop(stack)) 
                    | _ => start(temp,pop(stack))  
                    end ) 
        else let val char = Option.getOpt(option,#"%") 
            in case char of 
            #" "  => (let val o1 = write(implode(rev(tl(actual)))) ; val o2 = write("</u>"); val callee = head(tl(stack));  
                     in case callee of 
                      "<h1>" => heading(char::nil,pop(stack),1,true) | "<h2>" => heading(char::nil,pop(stack),2,true) |"<h3>" => heading(char::nil,pop(stack),3,true) 
                       |"<h4>" => heading(char::nil,pop(stack),4,true) |"<h5>" => heading(char::nil,pop(stack),5,true) |"<h6>" => heading(char::nil,pop(stack),6,true)  
                    | "<strong>" => strong_opening(temp,pop(stack),char::nil) 
                    | "<em>" => em_opening(temp,pop(stack),char::nil)
                    | "<ul>" => unordered_list_item(char::nil,pop(stack)) 
                    | "<ol>" => ordered_list(char::[],pop(stack)) 
                    | _ => start_helper(temp, pop(stack),char) 
                    end )
            | #"\n"  => (let val o1 = write(implode(rev(tl(actual)))) ; val o2 = write("</u>"); val callee = head(tl(stack));  
                     in case callee of 
                     "<h1>" => (let val o1 = write(getheading(2,1)) in start_helper([],pop(pop(stack)),char) end)
                    | "<h2>" => (let val o1 = write(getheading(2,2)) in start_helper([],pop(pop(stack)),char) end)
                    | "<h3>" => (let val o1 = write(getheading(2,3)) in start_helper([],pop(pop(stack)),char) end)
                    | "<h4>" => (let val o1 = write(getheading(2,4)) in start_helper([],pop(pop(stack)),char) end)
                    | "<h5>" => (let val o1 = write(getheading(2,5)) in start_helper([],pop(pop(stack)),char) end)
                    | "<h6>" => (let val o1 = write(getheading(2,6)) in start_helper([],pop(pop(stack)),char) end)
                    | "<ul>" => unordered_list_item_check(char::[],pop(stack)) 
                    | "<ol>" => ordered_list_item_check([],pop(stack)) 
                    | "<strong>" => strong_opening(temp,pop(stack),char::nil) 
                    | "<em>" => em_opening(temp,pop(stack),char::nil)     
                    | _ => start_helper(temp, pop(stack),char) 
                    end )
        | #"*" => (let val o1 = write(implode(rev(tl(actual)))) ; in em_opening_check([],stack,[]) end )
        |   #"_" => raise underline_error 
         | #"#" => raise heading_in_underline_error 
        
        |   _ => underline(temp,stack,char::actual) 
        end 
    end 


and unordered_list_check(temp,stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then raise list_error 
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of   
            #" " => (if (head(stack) = "<p>") then 
                    let val o2 = write(implode(rev(temp))) ; 
                        val o1 = write("</p>\n");
                        val o1 = write("<ul>") ;  in 
                    unordered_list_item([],"<ul>" :: stack) 
                    end 
                    else   
                    let 
                        val o1 = write("<ul>\n") ; val o2 = write("<li>") ;  in 
                    unordered_list_item([],"<ul>" :: stack) 
                    end ) 
        |   #"\n" => start_helper(#"-" :: temp,stack,char) 
        |   _ => start(#"-" :: char :: temp, stack) 
            end 
    end 

and unordered_list_item_generator(temp,stack)  = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then raise list_error 
        else 
            let val char = Option.getOpt(option,#"%") ;
            in case char of  
           #" " =>  (let val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o2 = write("<li>"); in 
                        unordered_list_item([],stack) end )
        |  #"*" =>  (let val o1 = write(implode(rev(#"-" :: #"\n" :: temp))); in em_opening_check([],stack,[]) end)  
        | #"_" => (let val o1 = write(implode(rev(#"-" :: #"\n" ::temp))); val o2 = write("<u>");in underline([],push("<u>",stack),[]) end)  

        |   _ => unordered_list_item(char::(#"-")::(#"\n")::temp,stack) 
        end 
    end            

and  unordered_list_item_check(temp,stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then  (let val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ul>\n")  
                    in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #"\n" => (let val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ul>\n")  
                    in start_helper([],pop(stack),char) end ) 
        |  #"#" => (let  val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ul>\n")  
                    in heading([],pop(stack),1,false) end )
        |   #"-" => unordered_list_item_generator(temp,stack) 
        |  #"*" =>  (let val o1 = write(implode(rev(#"\n" :: temp))); in em_opening_check([],stack,[]) end)  
        | #"_" => (let val o1 = write(implode(rev(#"\n" :: temp)));val o2 = write("<u>"); in underline([],push("<u>",stack),[]) end)  

        |   _ => unordered_list_item(char::(#"\n")::temp,stack)
            end 
    end  
            
and unordered_list_item(temp,stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (let val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ul>\n")  
                    in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of  
            #"\n" => unordered_list_item_check(temp,stack) 
        | #"_" => (let val o1 = write(implode(rev(temp)));val o2 = write("<u>"); in underline([],push("<u>",stack),[]) end)  
        | #"*" => (let val o1 = write(implode(rev(temp))); in em_opening_check([],stack,[]) end )  
        |   _ => unordered_list_item(char :: temp,stack) 
            end 
    end 



and ordered_list_check_dot(temp,stack) =  let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then start(temp,stack)  
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of  
            #"." => ordered_list_check_space(temp,stack)  
        |   _ => start(char::temp,stack) 
            end             
    end 
and ordered_list_check_space(temp,stack) =  let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then start((#".") :: temp,stack)  
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of  
            #" " => (let val o1 = write(implode(rev(tl(temp)))) ; val o2 = write("\n<ol>") ; val o3 = write("<li>") ;in 
                    ordered_list([],"<ol>" :: stack) 
                    end)   
            | _ => start(char::temp,stack)  
            end             
    end 

and ordered_list(temp,stack) =  let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then   (let  val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n")  
                    in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of  
            #"\n" => ordered_list_item_check(temp,stack) 
        |   #"*" => (let val o1 = write(implode(rev(temp))); in em_opening_check([],stack,[]) end ) 
        | #"_" => (let val o1 = write(implode(rev(temp))); val o2 = write("<u>");in underline([],push("<u>",stack),[]) end)  

        |   _ => ordered_list(char :: temp,stack) 
            end 
    end 

and ordered_list_item_check(temp,stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then  (let val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n")  
                    in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #"\n" => (let  val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n")  
                    in start_helper([],pop(stack),char) end ) 
        |  #"#" => (let  val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n")  
                    in heading([],pop(stack),1,false) end )
        |   #"*" => (let val o1 = write(implode(rev((#"\n")::temp))); in em_opening_check([],stack,[]) end ) 
        |  #"_" => (let val o1 = write(implode(rev((#"\n")::temp))); val o2 = write("<u>");in underline([],push("<u>",stack),[]) end)  

        |   #"1" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)  |   #"2" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)|   #"3" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)|   #"4" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)   |   #"5" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack) 
        |   #"6" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)  |   #"7" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)|   #"8" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)|   #"9" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack)   |   #"0" => ordered_list_item_generator_dot(char::(#"\n")::temp,stack) 

        |   _ => ordered_list(char::(#"\n")::temp,stack)
            end 
    end  

(* temp has number also *)
and ordered_list_item_generator_dot(temp,stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then  (let  val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n");  
                    in start(hd(temp)::nil,pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #"." => ordered_list_item_generator_space(temp,stack) 
        (* |   _ =>  (let  val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n")  
                    in start(char::(hd(temp))::nil,pop(stack)) end ) *)
        |   #"*" => (let val o1 = write(implode(rev(temp))); in em_opening_check([],stack,[]) end ) 
        | #"_" => (let val o1 = write(implode(rev(temp))); val o2 = write("<u>");in underline([],push("<u>",stack),[]) end)  

        |   _ =>   ordered_list(char::temp,stack)
            end 
    end 

and ordered_list_item_generator_space(temp,stack) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then  (let val o1 = write(implode(rev(temp))); val o3 = write("</li>\n"); val o4 =write("</ol>\n");  
                    in start((#".") :: hd(temp)::nil,pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #" " => (let  val o2 = write(implode(rev(tl(tl(temp))))); val o3 = write("</li>\n");val o1=write("<li>") ; 
                    in  ordered_list([],stack) end )  
        |   #"*" => (let val o1 = write(implode(rev((#".")::temp))); in em_opening_check([],stack,[]) end ) 
        | #"_" => (let val o1 = write(implode(rev(#"."::temp))); val o2 = write("<u>");in underline([],push("<u>",stack),[]) end)  

            |   _ =>   ordered_list(char::(#".")::temp,stack)
            end 
    end 

and gen_blockquote(toggle,n) = if n = 0 then () else 
                                                if toggle = 1 then (let val o1 = write("<blockquote>\n") in gen_blockquote(toggle,n-1) end )
                                                else (let val o1 = write("</blockquote>\n") in gen_blockquote(toggle,n-1) end )

and blockquote_control(temp,stack,curr,nested) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (if head(stack) = "<p>" then let val o2 = write("</p>") ;val o1 = gen_blockquote(2,nested) in start([],pop(pop(stack))) end 
                        else let val o1 = gen_blockquote(2,nested) in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #">" => blockquote_control(temp,stack,curr + 1, nested)  
        |    #" " => blockquote_control(temp,stack,curr , nested)  
        |   #"\n" => (if curr = nested then (if head(stack) = "<p>" then let val o1 = write("</p>\n") in blockquote_closing([],pop(stack),curr,nested) end
                                              else blockquote_closing([],stack,curr,nested) )   
                        else if curr < nested then if head(stack) = "<p>" then (let val o2 = write("</p>\n") ;val o1 = gen_blockquote(2,nested-curr);  in blockquote_closing([],pop(stack),curr,curr) end ) 
                                                    else  (let val o1 = gen_blockquote(2,nested-curr);  in blockquote_closing([],stack,curr,curr) end )
                        else if head(stack) = "<p>" then (let val o2 = write("</p>\n") ;val o1 = gen_blockquote(1,curr-nested);  in blockquote_closing([],pop(stack),curr,curr) end ) 
                                                    else  (let val o1 = gen_blockquote(1,curr-nested);  in blockquote_closing([],stack,curr,curr) end ))
        
        |   _ => if curr <= nested then (if head(stack) = "<p>" then blockquote_text(char::[],stack,curr,nested) 
                                        else (let val o1 = gen_blockquote(2,nested-curr) ; val o2 = write("<p>") ; in blockquote_text(char::[],push("<p>",stack),curr,curr) end))
                  (* else if curr = nested then  if  head(stack) = "<p>" then blockquote_text(char::[],stack,curr,nested)
                                              else  *)
                  else (if head(stack) = "<p>" then let val o2 = write("</p>\n") ;val o1 = gen_blockquote(1,curr-nested); val o2 = write("<p>"); in blockquote_text(char::nil,stack,curr,curr) end   (* pop then push <p> *)
                        else (let val o1 = gen_blockquote(1,curr-nested); val o2 = write("<p>"); in blockquote_text(char::nil,push("<p>",stack),curr,curr) end ))
            end     
    end 
and blockquote_text(temp,stack,curr,nested) =  let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then (if head(stack) = "<p>" then let val o2 = write("</p>") ;val o1 = gen_blockquote(2,nested) in start([],pop(pop(stack))) end 
                        else let val o1 = gen_blockquote(2,nested) in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
            #"\n" => (let val o1 = write(implode(rev((#"\n")::temp))) ; in blockquote_closing([],stack,curr,nested) end ) 
        |   _ => blockquote_text(char::temp,stack,curr,nested)
            end 
    end 

and blockquote_closing(temp,stack,curr,nested) = let 
        val option = TextIO.input1(new_input) ;val test = Option.isSome(option) ; 
    in 
        if (not test) then  (if head(stack) = "<p>" then let val o2 = write("</p>") ;val o1 = gen_blockquote(2,nested) in start([],pop(pop(stack))) end 
                        else let val o1 = gen_blockquote(2,nested) in start([],pop(stack)) end )
        else 
            let val char = Option.getOpt(option,#"%") 
            in case char of 
           
           #">" => blockquote_control([],stack,1,nested) 
        |   #" " => blockquote_closing(temp,stack,curr,nested)
        |   _    => (if head(stack) = "<p>" then let val o2 = write("</p>") ;  val o1 = gen_blockquote(2,nested) in start_helper([],pop(pop(stack)),char) end 
                        else let val o1 = gen_blockquote(2,nested) in start_helper([],pop(stack),char) end ) 
            end 
    end      
            
in  
fun convert() = start([],[]) ; 
end 



