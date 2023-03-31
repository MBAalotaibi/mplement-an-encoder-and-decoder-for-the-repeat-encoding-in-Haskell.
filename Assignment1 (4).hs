-- Do not alter the following line
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char, drop_char, encode, complex_encode, complex_decode) where


-- Part A

char_to_int :: Char -> Integer
char_to_int '0' = 0  --nameing each integer charcter to pure integer number
char_to_int '1' = 1  --nameing each integer charcter to pure integer number
char_to_int '2' = 2  --nameing each integer charcter to pure integer number
char_to_int '3' = 3  --nameing each integer charcter to pure integer number
char_to_int '4' = 4  --nameing each integer charcter to pure integer number
char_to_int '5' = 5  --nameing each integer charcter to pure integer number
char_to_int '6' = 6  --nameing each integer charcter to pure integer number
char_to_int '7' = 7  --nameing each integer charcter to pure integer number
char_to_int '8' = 8  --nameing each integer charcter to pure integer number
char_to_int '9' = 9  --nameing each integer charcter to pure integer number
char_to_int  if_not_correct= error "Not implemented1" --if user puts any other number than defined above


repeat_char :: Char -> Integer -> String
repeat_char c 0 = []  -- defining base case 
repeat_char c n = c :  repeat_char c (n-1)  -- repeat_char has two inputs c and n where c is the character and n is the integer, we use recursion here to run when (n-1) = 0 using base case




decode :: String -> String
decode [] = []
decode (x:y:xs) = (repeat_char x (char_to_int y)) ++ decode xs  --break down the string into x:y:xs, and converting second element which is char and convert it to integer, and then passing it to repeat_char code(question 2).



-- Part B

int_to_char :: Integer -> Char
int_to_char 0 = '0'--defining each integer number to pure integer character
int_to_char 1 = '1'--defining each integer number to pure integer character
int_to_char 2 = '2'--defining each integer number to pure integer character
int_to_char 3 = '3'--defining each integer number to pure integer character
int_to_char 4 = '4'--defining each integer number to pure integer character
int_to_char 5 = '5'--defining each integer number to pure integer character
int_to_char 6 = '6'--defining each integer number to pure integer character
int_to_char 7 = '7'--defining each integer number to pure integer character
int_to_char 8 = '8'--defining each integer number to pure integer character
int_to_char 9 = '9'--defining each integer number to pure integer character
int_to_char  11= error "Not implemented2"


length_char :: Char -> String -> Integer
length_char x [] = 0       -- defining base case when string is empty then results (Integer) will be zero
length_char x (y:ys)       --determining same letter in string	
    | x == y = 1 + length_char x ys  --it pattern match then 1 will be added to length_char and it calls itself again
    | otherwise = length_char x ys    --when upper condtion will false then this runs and program will be funished



drop_char :: Char -> String -> String
drop_char x [] = []   -- defining base case when string is empty then results (String) will be blank
drop_char x (y:ys) --determining same letter in string	
 |x == y = drop_char y ys  --it pattern match then it will be drop those letters it calls itself again
 |otherwise = y:ys --when upper condtion will false then this runs and program will be funished



encode :: String -> String
encode [] = []    -- defining base case when string is empty then results (String) will be blank
encode (x:xs) = (x : repeat_char(int_to_char (length_char x  (x:xs)  )) 1) ++ encode (drop_char x xs)   
-- start with length_char of string have same charcters, 
-- this string will be fed to int_to_char because type of length_char is integer and we need charcter, 
-- this character will be fed to repeat_char, we know repeat_char has two parameters to pass so will pass charcater received from above statment with the 1 to complete the parameters
-- after this encode will again call itself by droping those repeated character

   




-- Part C

complex_encode :: String -> String
complex_encode [] = []   -- defining base case when string is empty then results (String) will be blank
complex_encode [x] = [x] -- defining base case when string is charcater then results  will be same as input character
complex_encode (x:y:xs)   -- breaking the string 
 |x == y = (x : repeat_char (int_to_char(length_char x (x:y:xs))) 1) ++ complex_encode (drop_char x xs)  --using gaurd to make more condtions, when first character is equal to second charcater then the it question 7 will be repeated
 |otherwise =(x : complex_encode (y:xs))   --if character is not equal to succeeding character then it will simply call itself with the concatenation of first character
 
str_to_int :: String -> Integer
--str_to_int [] = [] -- defining base case when string is empty then results (String) will be blank
str_to_int [x] = char_to_int x  -- defining base case when string is charcater then results  will be same as input character
str_to_int (x:xs) = char_to_int x + (str_to_int xs*10) --this condtion refers to that when integer is greter then 9 then we multiply second integer with 10 and add with first integer to get a wholw number

get_str :: String -> String
get_str xs 
 |xs /= [] && head xs <= '9' && head xs >= '0' = head xs : get_str (tail xs) --xs will check three condtions and every condtion has to be true to run this expression
 --if xs is non-empty and its leading element is less than 9 and greater than 0 then the expression will run, which it will show head of xs and call get with the input of tail of xs
 |otherwise = [] --if one of above conditions fails then this will run 

drop_tailers :: String -> String
drop_tailers [] = []  -- defining base case when string is empty then results (String) will be blank
drop_tailers xs 
 |xs /= [] && head xs <= '9' && head xs >= '0' = drop_tailers(tail xs) --xs will check three condtions and every condtion has to be true to run this expression
  --if xs is non-empty and its leading element is less than 9 and greater than 0 then the expression will run, which it will drop head of xs and call get with the input of tail of xs
 |otherwise =  xs --if one of above conditions fails then this will run 



complex_decode :: String -> String
complex_decode [] = [] -- defining base case when string is empty then results (String) will be blank
complex_decode (x:xs) = repeat_char x (str_to_int  (reverse (get_str xs))) ++ complex_decode(drop_tailers xs)
--first we get string from xs. this string will fed to str_to_int to get integers, this integer will then reversed to change digits in an number, this reversed number then go into the repeat char with the first character x and this integer, we call complex_decode by droping the tailers


