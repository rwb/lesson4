### CCJS 710 - Lesson 4 - Thursday 9/22/22

* We begin today's lesson by further considering the Minneapolis data set:

```R
df <- read.csv(file="minn.txt",sep=",",header=T)
head(df,n=10)
```

* Here is the output:

```Rout
> df <- read.csv(file="minn.txt",sep=",",header=T)
> head(df,n=10)
   id ta td aggcirc y
1   1  1  1       1 1
2   2  1  1       1 1
3   3  1  1       1 1
4   4  1  1       1 1
5   5  1  1       1 1
6   6  1  1       1 1
7   7  1  1       1 1
8   8  1  1       1 0
9   9  1  1       1 0
10 10  1  1       1 0
> 
```

* Next, let's create a contingency table -- showing the joint distribution of the randomized treatment and the failure outcome.
* In addition, we will create a binary arrest treatment variable and the original treatment-as-assigned variable.

```R
table(df$y,df$ta,exclude=NULL)

df$arr <- rep(NA,313)
df$arr[df$ta==1] <- 1
df$arr[df$ta==2 | df$ta==3] <- 0
table(df$arr,df$ta,exclude=NULL)
```

* Here is the output:

```Rout
> table(df$y,df$ta,exclude=NULL)
   
     1  2  3
  0 82 87 87
  1 10 21 26
> 
> df$arr <- rep(NA,313)
> df$arr[df$ta==1] <- 1
> df$arr[df$ta==2 | df$ta==3] <- 0
> table(df$arr,df$ta,exclude=NULL)
   
      1   2   3
  0   0 108 113
  1  92   0   0
> 
```

* 
