exception end_of_file
fun add(x,y) = x + y ; 



local 
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

val l = [] ; 


fun start(temp,stack) = let 
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then () 
                    else  
                            let 
                             val char = Option.getOpt(option,#"%") 
                            in 
                            case char of 
                                #"#" => temp1(push(char,temp), stack)    
                            |   #"\n" => start(temp,stack)   
                            |   _ =>  if head(stack) = "<p>" then para_text(push(char,temp), stack) 
                                      else let  val o1 = write("<p>") in para_text(push(char,temp), push("<p>",stack)) end 
                             end    
                end 


and temp1 (temp, stack) = let   
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then let 
                                            val text = implode(rev(temp)) 
                                            val o3 = helper2(temp, stack)
                                            val o1 = write(text)  
                                             
                                            val o2 = write("</p>\n")
                                            in () 
                                        end   
                    else 
                        let 
                            val char = Option.getOpt(option, #"%") 
                        in 
                        case char of 
                            #" " => if head(stack) = "<p>" then 
                                                        let val o1 = write "</p>\n" in  
                                                        h1([], "<h1>" :: pop(stack))
                                                        end 
                                     else 
                                        let val o1 = write "" in 
                                        h1([], "<h1>" :: pop(stack))
                                        end 
                                         
                                         
                                      
                        |   #"#" => temp2(push(#"#",temp), stack) 
                        |   #"\n" => (let 
                                        val output = write "<h1></h1>" 
                                      in 
                                        start(temp, stack) 
                                    end ) 
                        |   _ => helper(char, temp, stack) 
                        end 
                end 
(* if stack empty or head not <p> then only <p> written  *)
and helper2(temp,stack as head :: tl) = ( if head <> "<p>" then write "<p>"   
                            else write ""   
                                     ) 
| helper2(temp, nil) = write "<p>" 
(* if stack empty or head not <p> then only <p> written  *)
and helper(char, temp, stack as head :: tl) =  ( if head <> "<p>" then let val output = write "<p>" in  para_text(char :: temp , "<p>" :: stack ) end 
                            else let val output = ()  in para_text(char :: temp , stack) end   
                                     ) 
| helper(char, temp, stack as nil) = let val output = write "<p>" in  para_text(char :: temp , "<p>" :: stack ) end 

and h1(temp,stack) =  let 
                              
                             val o1 = write("<h1>") 
                      in 
                            header_text(temp, stack,1)  
                      end 

and temp2 (temp, stack) = let   
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then let 
                                            val text = implode(rev(temp)) 
                                            val o3 = helper2(temp, stack)
                                            val o1 = write(text)  
                                             
                                            val o2 = write("</p>\n")
                                            in () 
                                        end   
                    else 
                        let 
                            val char = Option.getOpt(option, #"%") 
                        in 
                        case char of 
                            #" " => if head(stack) = "<p>" then 
                                                        let val o1 = write "</p>\n" in  
                                                        h2([], "<h2>" :: pop(stack))
                                                        end 
                                     else 
                                        let val o1 = write "" in 
                                        h2([], "<h2>" :: pop(stack))
                                        end 
                                         
                                         
                                      
                        |   #"#" => temp3(push(#"#",temp), stack) 
                        |   #"\n" => (let 
                                        val output = write "<h2></h2>" 
                                      in 
                                        start(temp, stack) 
                                    end ) 
                        |   _ => helper(char, temp, stack) 
                        end 
                end 

and h2(temp,stack) =   let 
                              
                             val o1 = write("<h2>") 
                      in 
                            header_text(temp, stack,2)  
                      end 
and temp3 (temp, stack) = let   
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then let 
                                            val text = implode(rev(temp)) 
                                            val o3 = helper2(temp, stack)
                                            val o1 = write(text)  
                                             
                                            val o2 = write("</p>\n")
                                            in () 
                                        end   
                    else 
                        let 
                            val char = Option.getOpt(option, #"%") 
                        in 
                        case char of 
                            #" " => if head(stack) = "<p>" then 
                                                        let val o1 = write "</p>\n" in  
                                                        h3([], "<h3>" :: pop(stack))
                                                        end 
                                     else 
                                        let val o1 = write "" in 
                                        h3([], "<h3>" :: pop(stack))
                                        end 
                                         
                                         
                                      
                        |   #"#" => temp4(push(#"#",temp), stack) 
                        |   #"\n" => (let 
                                        val output = write "<h3></h3>" 
                                      in 
                                        start(temp, stack) 
                                    end ) 
                        |   _ => helper(char, temp, stack) 
                        end 
                end 

and h3(temp,stack) =  let 
                              
                             val o1 = write("<h3>") 
                      in 
                            header_text(temp, stack,3)  
                      end
and temp4 (temp, stack) = let   
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then let 
                                            val text = implode(rev(temp)) 
                                            val o3 = helper2(temp, stack)
                                            val o1 = write(text)  
                                             
                                            val o2 = write("</p>\n")
                                            in () 
                                        end   
                    else 
                        let 
                            val char = Option.getOpt(option, #"%") 
                        in 
                        case char of 
                            #" " => if head(stack) = "<p>" then 
                                                        let val o1 = write "</p>\n" in  
                                                        h4([], "<h4>" :: pop(stack))
                                                        end 
                                     else 
                                        let val o1 = write "" in 
                                        h4([], "<h4>" :: pop(stack))
                                        end 
                                         
                                         
                                      
                        |   #"#" => temp5(push(#"#",temp), stack) 
                        |   #"\n" => (let 
                                        val output = write "<h4></h4>" 
                                      in 
                                        start(temp, stack) 
                                    end ) 
                        |   _ => helper(char, temp, stack) 
                        end 
                end 


and h4(temp,stack) =  let 
                              
                             val o1 = write("<h4>") 
                      in 
                            header_text(temp, stack,4)  
                      end
and temp5 (temp, stack) = let   
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then let 
                                            val text = implode(rev(temp)) 
                                            val o3 = helper2(temp, stack)
                                            val o1 = write(text)  
                                             
                                            val o2 = write("</p>\n")
                                            in () 
                                        end   
                    else 
                        let 
                            val char = Option.getOpt(option, #"%") 
                        in 
                        case char of 
                            #" " => if head(stack) = "<p>" then 
                                                        let val o1 = write "</p>\n" in  
                                                        h5([], "<h5>" :: pop(stack))
                                                        end 
                                     else 
                                        let val o1 = write "" in 
                                        h1([], "<h5>" :: pop(stack))
                                        end 
                                         
                                         
                                      
                        |   #"#" => temp6(push(#"#",temp), stack) 
                        |   #"\n" => (let 
                                        val output = write "<h5></h5>" 
                                      in 
                                        start(temp, stack) 
                                    end ) 
                        |   _ => helper(char, temp, stack) 
                        end 
                end 

and h5(temp,stack) =  let 
                              
                             val o1 = write("<h5>") 
                      in 
                            header_text(temp, stack,5)  
                      end
and temp6 (temp, stack) = let   
                    val option = TextIO.input1(new_input) ;
                    val test = Option.isSome(option) ; 
                in 
                    if (not test) then let 
                                            val text = implode(rev(temp)) 
                                            val o3 = helper2(temp, stack)
                                            val o1 = write(text)  
                                             
                                            val o2 = write("</p>\n")
                                            in () 
                                        end   
                    else 
                        let 
                            val char = Option.getOpt(option, #"%") 
                        in 
                        case char of 
                            #" " => if head(stack) = "<p>" then 
                                                        let val o1 = write "</p>\n" in  
                                                        h6([], "<h6>" :: pop(stack))
                                                        end 
                                     else 
                                        let val o1 = write "" in 
                                        h6([], "<h6>" :: pop(stack))
                                        end 
                                         
                                         
                                      
                        (* |   #"#" => temp2(push(#"#",temp), stack)  *)
                        |   #"\n" => (let 
                                        val output = write "<h6></h6>" 
                                      in 
                                        start(temp, stack) 
                                    end ) 
                        |   _ => helper(char, temp, stack) 
                        end 
                end 


and h6(temp,stack) =  let 
                              
                             val o1 = write("<h6>") 
                      in 
                            header_text(temp, stack,6)  
                      end

and header_text(temp,stack as head :: tl,count) = let val option = TextIO.input1(new_input) ;
                                        val test = Option.isSome(option) ; 
                                    in 
                                        if (not test) then let 
                                                        val text = implode(rev(temp)) 
                                                        val o1 = write(text)  
                                                        val o2 = write(getheading(count))
                                                    in () end  
                                        else  
                                                let 
                                                    val char = Option.getOpt(option,#"%") 
                                                in 
                                                    case char of 
                                                    #"\n" => (let 
                                                            val text = implode(rev(temp)) 
                                                            val o1 = write(text) 
                                                            val ol2 = write(getheading(count))
                                                        in 
                                                            start([], tl) 
                                                        end ) 
                                                |   _ => header_text(char :: temp, stack, count) 
                                                end 
                                    end 

and getheading(count) = case count of 
                        1 => "</h1>\n" 
                    |   2 => "</h2>\n"
                    |   3 => "</h3>\n"
                    |   4 => "</h4>\n"
                    |   5 => "</h5>\n"
                    |   6 => "</h6>\n"
                    |   _ => "</h6>\n"

and para_text(temp,stack as head :: tl) = let 
                                
                                val option = TextIO.input1(new_input) ;
                                val test = Option.isSome(option) ;
                            in 
                                 if (not test) then 
                                                    let 
                                                        val text = implode(rev(temp)) 
                                                        val o1 = write(text)  
                                                        val o2 = write("</p>\n")
                                                    in () end  
                                 else  
                                    let 
                                    val char = Option.getOpt(option,#"%") 
                                    in 
                                    case char of 
                                        #"\n" => (let 
                                                        val text = implode(rev(temp)) 
                                                        val o1 = write(text^"\n")   
                                                        (* val o2 = write("</p>\n") *)
                                                    
                                                in 
                                                    start([], stack)
                                                end ) 
                                    |    _ => para_text(char::temp, stack) 
                                     
                                    end 
                            end    
                                
in  

fun convert() = start([],[]) ; 

end 

























(* 

fun scan_helper ans = 
    let        
    val str = TextIO.inputLine new_input ;
    val line = Option.getOpt(str, "") ;
    
                
    in 
        if (line <> "") then 
            let 
            val temp = explode(line) 
            val temp2 = append(temp,ans) 
            val vec = Vector.fromList temp2 ; 
            in 
            TextIO.output(new_output, line) ;
            scan_helper temp2  
            end 
        else 
            let 
            val t1 = TextIO.closeIn new_input ; 
            (* val t2 = TextIO.flushOut new_output ;  *)
            val t3 = TextIO.closeOut new_output ;  
            in 
            ans 
            end 
    end ;              


fun scan dummy = scan_helper [] ; *)
