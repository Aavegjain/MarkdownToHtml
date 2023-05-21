# Design decisions
(for the problem statement check the HTML file attached)
1. newlines -- handled as in markdown ( 2 newlines terminate a p tag) 

2. heading level more than 7 is a error 

3. inside emphasis, only italics is allowed (\*\* \*text\* \*\*) (bold inside bold senseless) . Likewise inside italics only bold is allowed (\* \*\* \*\* \*) 

4. if \*\* \* then error, \* \*\* then also 
\*\*\* not allowed

5. \*\*EOF not allowed ; \*EOF not allowed
 also, headings not allowed in bold, em.

6. for underlines, if u want space, have to do _text_text_; _text'space' results in error 

7. underline cant be inside bold, italics; 
    but vice versa is possible

8. A blank line means the end of a list 

9. a list needs to be closed by a blankline(or newline if at eof) otherwise error
 

10. a list occurs in the outermost scope by default and no nested lists

hr needs a terminating newline

11. have handled blockquotes as on dingus 

12. format of tables decided   
<<newline  
text|text|text  
text|text|text  
....  
\>>newline   
13. in each row different number of cells can occur 

14. assuming link is present only in one line. thus 
[link_text](text \n   is an error

15. have handled escape characters for outermost scope , and for . of ordered lists.
