# R program for Transpose of a Matrix

# create a matrix with 2 rows
# using matrix() method
r=as.integer(readline("enter no: of rows: "))
c=as.integer(readline("enter no: of column: "))
M <- matrix(1:6, nrow = 2)

# print the original matrix
print(M)

# transpose of matrix
# using t() function.
t <- t(M)

# print the transpose matrix
print(t)

# create a 2 by 3 matrix
# matrix1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)

# print(matrix1)



///Concatenate series
# create two strings

#Quadratic

quadratic<-function(a,b,c){
    print(paste("enter the quadratic equtaion:",a,(x^2),b,(x),c))
    root<- (b^2)-(4*a*c)
    if(root<0){
        print(paste("no roots"))
    }
    else if (root>0) {
       x_plus = (-b + sqrt(root)) / (2*a)
       x_neg = (-b - sqrt(root)) / (2*a)
       print(paste("possitive solution is:",x_plus ,"negative solution is:",x_neg))
    }
    else{
        z = -b / (2*a)
        print(paste("the solution is:",z))
    }
}
a=as.integer(readline("enter the value of x:"))
b=as.integer(readline("enter the value of y:"))
c=as.integer(readline("enter the value of z:"))
quadratic(a,b,c)

#merge

# merging two datasets 
authors <- data.frame( 
name = c("kapil", "sachin", "Rahul","Nikhil","Rohan"), 
nationality = c("US","Australia","US","UK","India"), 
retired = c("Yes","No","Yes","No","No")) 

books <-data.frame( 
name = c("C", "C++","Java","php",".net","R"), 
title = c("Intro to C","Intro to C++", 
			"Intro to java", "Intro to php", 
			"Intro to .net", "Intro to R"), 
author = c("kapil", "kapil","sachin", "Rahul", 
			"Nikhil","Nikhil")) 
	
m<-merge(authors, books, by.x = "name", by.y = "author") 
print(m)

string1 <- "Basket"
string2 <- "ball"
 
#  string1=as.integer(readline("enter string1: "))
#  string2=as.integer(readline("enter string2: "))
# using paste() to concatenate two strings
result = paste(string1, string2)
 
print(result)





//factorial an ncr

# take input from the user
num = as.integer(readline(prompt="Enter a number: "))
factorial = 1
# check is the number is negative, positive or zero
if(num < 0) {
print("Sorry, factorial does not exist for negative numbers")
} else if(num == 0) {
print("The factorial of 0 is 1")
} else {
for(i in 1:num) {
factorial = factorial * i
}
print(paste("The factorial of", num ,"is",factorial))
}

# R program to calculate nCr value 
	
# Using choose() method 
n=as.integer(readline("enter n: "))
r=as.integer(readline("enter r: "))
answer1 <- choose(n, r) 
print(paste(answer1))





//prime numbers

l = as.integer(readline("Enter first number:"))
h = as.integer(readline("Enter first number:"))

is_prime <-  function(num) {
  if (num <= 1) {
     return(FALSE)
  } 
  for (i in 2:sqrt(num)) {
     if (num %% i == 0) {
        return(FALSE)
     }
  }
  return(TRUE)
}

for (i in l:h) {
   if (is_prime(i)) {
      print(i)
   }
}



//odd/even
a=as.integer(readline("Enter a number: "))
if (a%%2==0){
    print(paste("even"))
}else{
    print(paste("odd"))
}

//largeat

a=as.integer(readline("Enter first number: "))
b=as.integer(readline("Enter second number "))
c=as.integer(readline("Enter third number "))
if (a>b && a>c){
    print(paste(a, "is greater"))
}else if(b>c && b>a){
    print(paste(b,"is greater"))
    
}else{
    print(paste(c, "is greater"))
}









///sum of n

# take input from the user
num = as.integer(readline(prompt = "Enter a number: "))
sum =0
for (i in seq(0,num)){
    sum=sum+i
}
print(paste(sum))





//Fibonacci 

fibonacci <- function(n) {
if (n <= 0) {
	return(NULL)
} else if (n == 1) {
	return(0)
} else if (n == 2) {
	return(1)
} else {
	return(fibonacci(n - 1) + fibonacci(n - 2))
}
}

print_fibonacci_sequence <- function(n) {
if (n <= 0) {
	cat("Invalid input. Please enter a positive integer.\n")
	return()
}

cat("Fibonacci Sequence:")
for (i in 1:n) {
	cat(" ", fibonacci(i))
}
cat("\n")
}

# Change the value of 'n' to the desired number of terms in the sequence
# n <- 10
n=as.integer(readline("enter n: "))
print_fibonacci_sequence(n)



//String reverse


reverseStr <- function(str) {
reversedStr <- " "
while (nchar(str) > 0) {
	reversedStr <- paste0(reversedStr, substr(str, nchar(str), nchar(str)))
	str <- substr(str, 1, nchar(str) - 1)
}
return(reversedStr)
}

# Example usage
# str <- "Hello, R!"
str=readline("enter the string")
reversedStr <- reverseStr(str)
print(reversedStr)


# R program for combining two matrices
# column-wise

# Creating 1st Matrix
B = matrix(c(1, 2), nrow = 1, ncol = 2) 

# Creating 2nd Matrix
C = matrix(c(3, 4, 5), nrow = 1, ncol = 3)

# Original Matrices
print(B)
print(C)

# Combining matrices
print (cbind(B, C))


#Transpose
r=as.integer(readline("enter the no of rows:"))
c=as.integer(readline("enter the no of columns:"))
ME=scan()
n=matrix(c(ME),nrow=r,ncol=c ,byrow=TRUE)
cat("the matrix is\n")
print(n)
cat("transpose is\n")
print(t(n))

#join
r1=as.integer(readline("Enter the number of rows of 1st matrix:"))
c1=as.integer(readline("Enter the number of columns of 1st matrix:"))
ME=scan()
A=matrix(c(ME),nrow=r1,ncol=c1 ,byrow=TRUE)
cat("the matrix is\n")
print(A)
r2=as.integer(readline("Enter the number of rows of 2nd matrix:"))
c2=as.integer(readline("Enter the number of columns of 2nd matrix:"))
ME=scan()
B=matrix(c(ME),nrow=r2,ncol=c2 ,byrow=TRUE)
cat("the matrix is\n")
print(B)

print(rbind(A,B))
print(cbind(A,B))


#area of a circle

n=as.integer(realine("enter the num:"))
area=3.14*r*realine
        print(paste("area of circle:",area))
  


#factors of number

n=as.integer(realine("enter the num:"))
for(i in 1:n){
    if(n%%i==0){
        print(i)
    }
}


##Linear regression in Calc

Algorithm 
Step 1 : Start 
Step 2 : Enter data sample 
Step 3 : Select entire data sample 
Step 4 : Select insert from Menu bar and Click on chart 
Step 5 : Choose XY (Scatter) as chart type 
Step 6 : Select points only 
Step 7 : Click Next-> Next-> Next 
Step 8 : In chart elements give Title and axis labels 
Step 9 : Click Finish 
Step 10 : Select a point on the appeared scatter plot 
Step 11 : Select insert from Menu bar and Click on Trend line 
Step 12 : Select Type and choose linear as regression type 
Step 13 : Check Show Equation and click ok 
Step 14 : Using the Equation Predict Avg Expense 
Step 15 : Stop



## Histogram in Calc

Algorithm 
Step 1 : Start 
Step 2 : Enter data sample 
Step 3 : Calculate min and max of sample data using min() & max() 
Step 4 : Choose lower bound of min value as starting value of bin, Choose Upper bound of max value 
as ending value of bin 
Step 5 : Select all cells next to bin values to calculate frequency 
Step 6 : Calculate frequency using the Equation “=Frequency(Data;class)” (Where data is the sample 
data and class is bin values) 
Step 7 : After entering equation click Shift+Ctrl+Enter to execute 
Step 8 : Select Entire bin and Frequency(Including Labels) 
Step 9 : Select insert from Menu bar and Click on chart 
Step 10 : Choose Column as chart type and click next 
Step 11 : Check Data series in columns, First row as label and First column as label 
Step 12 : Click next->next 
Step 13 : In chart elements give Title and axis labels
Step 14 : Click Finish 
Step 15 : Stop

## Logistic Regression 

Program
library("mlbench")
library("ggplot2")
library("magrittr")
data(PimaIndiansDiabetes2)
model <- glm( diabetes ~ glucose, data = 
PimaIndiansDiabetes2[1:100,], family = binomial)
summary(model)
newdata <- data.frame(glucose = c(20, 30, 40, 60, 80, 
100,120,160,180))
prob<-model %>% predict(newdata, type = "response")
res<-ifelse(prob > 0.5, "pos", "neg")
res


##Linear Regression

library("ggplot2")
model <- lm( mpg ~ cyl, data = mtcars[1:100,])
summary(model)
ggplot(mtcars,aes(cyl,mpg))+geom_point(aes(colour=factor(cyl),
shape=factor(cyl)),size=4)+geom_smooth(method="lm")


## Mean, Median, Mode


data=read.csv("Rainfall.csv")
rainfall=data$Rainfall
mode=function(Rainfall) {
return(names(sort(-table(Rainfall))[1]))
}
print(paste("The mean is ",mean(rainfall)))
print(paste("The median is ",median(rainfall)))
print(paste("The mode is ",mode(rainfall)))

