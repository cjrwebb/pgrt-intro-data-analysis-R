
#############################################################
####### An Introduction to Data Analysis Using R ############
##### Dr. Calum Webb, the University of Sheffield ###########
#############################################################

# 1. A quick tour of RStudio. -------------------------------

#' If you can read this, it means you've successfully loaded
#' up R and RStudio, and then opened this file (called an 
#' R Script). I'll give a quick rundown at the front of what
#' each of the parts of RStudio do, but this is written below
#' so you can review.
#' 
#' Let's start with the script and the top-left pane: this is
#' where you would write your R code, and any comments to
#' yourself (this is a comment, it starts with a #, you can 
#' use #' in order to automatically continue your comment onto
#' a new line when you press enter.)
#' 
#' In the top right is your 'environment'. This pane shows
#' what data and objects you have loaded into R. You can see
#' an "Import Dataset" button on it. Don't use it. It's better
#' to read your data into R using code. We'll go through that
#' later.
#' 
#' In the bottom right you should see the files that are in
#' this R project (providing you opened the "blue box" R
#' project file that is called pgrt-intro-data-analysis-r).
#' This is also where your plots will appear.
#' 
#' At the bottom left is your console; this is where you can
#' run code that you don't need to keep a record of (i.e.
#' if you're just testing something or checking something).


# 2. How on earth do I code!? ----------------------------

#' It's not as difficult as it sounds. The key thing to 
#' remember when you're first learning to code is this:
#' there are things that you need to change and things
#' that you don't need to change, in order to get the 
#' code to do what you want. 

# 3. Functions ------------------------------------------

#' Functions are things that tell R what to do. Functions
#' always end with brackets after the word (). For example,
#' let's take a look at the sample() function. Try running
#' the code below and see what happens. You can run the 
#' code by either highlighting it, or clicking anywhere on
#' it, and then clicking the "Run" button at the top of the
#' script pane, or you can click anywhere on the code and 
#' then press Ctrl + Enter (or Command + Enter on a Mac).

sample()

#' We get an error, R is telling us that the function sample()
#' requires an argument called x which is missing, and doesn't
#' have a default. Let's try adding a value for x.

sample(x = 1)

#' Arguments have names that always stay the same and are 
#' followed by a single = sign. On the right hand side of
#' the equals is something that *changes* depending on what
#' you want the function to do.
#' 
#' Okay, now the function is doing something. But *what* is it
#' doing? And how do we know what arguments we can use in the
#' function.

#' Under sample() we have another argument we can use called
#' size. Let's see what happens if we run the following code:

sample(x = 6, size = 1)

#' With this code we are telling R to sample 1 number (size =
#' 1) from a choice of numbers up to 6 (x = 6). Note that
#' each argument is separated by a comma.


# 4. Assignment and other operators ------------------------

#' You'll have noticed that when we run this code the result
#' appears in the console below and then is effectively 
#' forgotten. But what if we want to save the result and then
#' use it again later? We can use the assignment operator in
#' R to store a value, this will save it to the environment.
#' 
#' For example, let's roll our dice twice and save both
#' results. We'll save the first result in an object called
#' "first_roll" and the second result in an object called
#' "second_roll". These could be called anything you want
#' though; the *name that you give an object is arbitrary*,
#' unlike the name of the function or the argument, which
#' are always the same.

first_roll <- sample(x = 6, size = 1)
second_roll <- sample(x = 6, size = 1)

#' Note that when we save a result to an object, we don't
#' get a print out of that result in the console. We just
#' get a copy of the code that was run. If we want to see
#' the result, we need to just run the object in the same
#' way we ran our function.

first_roll
second_roll

#' Now that these results are saved, we can use them for
#' additional things. For example, we could add the roles
#' together:

first_roll + second_roll

#' We could divide the first roll by the second roll:

first_roll / second_roll

#' Or we could perform a logic operation such as finding
#' out whether the first roll was greater than the second
#' roll or not:

first_roll > second_roll

#' There's a lot more we could talk about when it comes to
#' R code, but the focus on today's session is about data
#' analysis. 


# 5. Using Datasets ---------------------------------------

#' R has some built in datasets that we can start off using
#' to illustrate some examples. We'll talk about how to read
#' in data in a little while. For now, we're going to use the
#' two related datasets ChickWeight and chickwts to illustrate
#' data analysis in R. These are about the growth of baby
#' chickens over time, but that doesn't matter right now.

ChickWeight
chickwts

#' These are what we would call "tidy" datasets, meaning
#' that each row contains an observation (a chick at a 
#' specific time) and each column contains a variable.
#' 
#' If we want to reference a variable within a dataset,
#' we use a dollar sign $ after the name of the dataset,
#' and then type in the variable's name:

ChickWeight$weight

#' Now we can apply our functions to the variables in our
#' dataset in order to do data analysis. 


# 6.1 Univariate Descriptive Statistics ----------------

#' Base R has some fairly straightforward functions for
#' generating simple descriptive statistics. For example,
#' if we want to know the mean and the standard deviation
#'  of a variable we can use:

mean(x = ChickWeight$weight)
sd(x = ChickWeight$weight)

#' IMPORTANT NOTE: for most real-world data, you will 
#' have missing values. When using many functions in R
#' there is an optional argument called na.rm, which is
#' an argument to remove all missing values before
#' calculating the statistic. You would generally want
#' to set this to TRUE (in all caps) to remove the 
#' missing data, i.e.:

mean(x = ChickWeight$weight, na.rm = TRUE)
sd(x = ChickWeight$weight, na.rm = TRUE)


#' Some other functions can be a little more tricky to 
#' write the arguments for, for example, if we wanted
#' to calculate an 80% quantile range for a variable:

quantile(x = ChickWeight$weight, probs = c(0.1, 0.9))

#' Here you can see we have a "function in a function",
#' first, we have the quantile function, with the argument
#' for x set to ChickWeight$weight, but then for the argument
#' probs, we have what is called a vector (within the c()
#' function) with two values separated by a column. Try
#' copy and pasting the above code and then changing the
#' probs value to something different, like a 50% range
#' centred on the middle value.



#' If we wanted a collection of helpful descriptive 
#' statistics for our variable, we could actually use 
#' the summary() function:

summary(ChickWeight$weight)

#' Summary is a special function in R that is designed
#' to work differently with different things it is given.
#' When provided with a numeric variable, it generates 
#' the mean, median, mode, min, max, and inter-quartile
#' range. Let's try using summary on the "feed" variable
#' (a categorical variable):

summary(chickwts$feed)

#' We get something that is almost like a table (it's 
#' actually something called a named vector). If we wanted
#' an actual table we could try:

table(chickwts$feed)


#' EXERCISE: Using what we've learned, try to calculate
#' the median for the variable weight in the ChickWeight
#' dataset below.




# 6.2 Creating a new variable ---------------------------

#' Before we demonstrate bivariate descriptive statistics,
#' let's talk about how we could create a new variable.
#' For example, let's imagine we want to create a variable
#' that represents whether a chick is in the top 10% of 
#' chicks by weight. Let's get the 90th percentile for
#' the chicks in the chickwts dataset:

quantile(x = chickwts$weight, probs = c(0.1, 0.9))

#' Now let's apply a logic operation to the weights variable,
#' if the value is over 359, we want the new variable to be
#' TRUE.

chickwts$weight > 359

#' As before, this calculation isn't saved anywhere. If we 
#' want to save it to a new variable, we need to assign it.
#' But, what is different here is that we'll be assigning it
#' to a new variable name *in the dataset*.

chickwts$big <- chickwts$weight > 359

#' We're creating a new variable here, inside chickwts, that
#' is called big, which has a TRUE or FALSE value depending 
#' on if the chick is over 359g or not.


# 6.3 Bivariate descriptive statistics ---------------------

#' Now we have two categorical variables in the chickwts
#' dataset, "big" and "feed", which we can use to create
#' a crosstab (contingency table) with table(). Note that
#' when two variables are provided to table, the first
#' variable becomes the rows while the second variable
#' becomes the columns. 

table(chickwts$big, chickwts$feed)

#' How about our continuous variables? This is where base R
#' code starts to get a little bit unweildy. We *can* do 
#' this fairly straightforwardly with the following 
#' aggregate function, but it's not always so each to read
#' what is going on here:

aggregate(chickwts$weight, by = list(chickwts$feed), mean)
aggregate(chickwts$weight, by = list(chickwts$feed), sd)

#' We'll talk about some alternatives when we visit other
#' R packages and reading in data in the second half
#' of the training session.


# 7. Basic Data Visualisation --------------------------

#' We can perform some basic data visualisation in R for
#' the variables we have using built in functions. For 
#' example, to create a histogram, we can use:

hist(x = chickwts$weight)

#' To create a bar chart, we can provide the barplot()
#' function with a table:

barplot(table(chickwts$feed))

#' Now, let's consider a couple of common bivariate 
#' data visualisations. We can use the built in boxplot()
#' function to create boxplots:

boxplot(chickwts$weight ~ chickwts$feed)

#' Note that here the argument is what is called a "formula",
#' that is, on the left and side of the tilde (~) is the 
#' continuous variable that makes up our Y axis, and on the
#' right hand side of our tilde (~) is our "grouping" 
#' variable.

#' Scatterplots are perhaps so unanimous a tool that they
#' are created through the basic plot() function:

plot(x = ChickWeight$Time, y = ChickWeight$weight)

#' There are many, many, other types of plots such as
#' line plots, but this is where base R starts to fall
#' down a little bit. For example, if we wanted to add
#' lines for each chick to the above plot, we would have
#' to run this quite complicated bit of code:

for (i in 1:max(ChickWeight$Chick)) {
  lines(x = ChickWeight[ChickWeight$Chick == i, ]$Time, 
        y = ChickWeight[ChickWeight$Chick == i, ]$weight)
}

#' That's pretty complicated, not to mention if we wanted
#' to change the lines to different colours, etc.

#' Luckily, we're going to look at a *much* better way
#' to do this later on using an R package called ggplot2.


# 8. Some basic inferential statistics -----------------

#' As a statistical programming language, R can also, of 
#' course, perform statistical tests and give you the 
#' results. Let's take out contingency table from above:

table(chickwts$big, chickwts$feed)

#' If we wanted to run a Chi Square text on the above 
#' table, we could use the following function:

chisq.test(table(chickwts$big, chickwts$feed))

#' Here our table is *inside* the chisq.test function.
#' The results can be interpreted as they would in any
#' statistical software.

#' Similarly, we have functions for correlation, both
#' Pearson's R or Spearman's rho:

cor.test(ChickWeight$weight, 
         ChickWeight$Time, 
         method = "pearson")

cor.test(ChickWeight$weight, 
         ChickWeight$Time, 
         method = "spearman")

#' And lastly, one can use a t-test in R, as
#' with any statistical software:

t.test(x = chickwts$big, y = chickwts$weight)

#' However, ANOVA tests work a little bit differently in that
#' you first need to estimate a linear model using lm, with
#' a similar formula to how we created our boxplot, and
#' save the result:

lm_result <- lm(formula = chickwts$weight ~ chickwts$feed)

#' and then use the resulting object in the aov() function
#' to find out if any group was significantly different to
#' any other group. You will need to use summary on the result
#' afterwards in order to get a p-value

anova_result <- aov(lm_result)
summary(anova_result)

#' You can also follow this up with a Tukey HSD test to see
#' which groups were significantly different from one another.
#' The Tukey 

TukeyHSD(anova_result)

#' If you wanted to see the result of a linear regression,
#' you can use summary() on the one we created earlier:

summary(lm_result)

#############################################################
##########################  Part 2 ##########################
#############################################################

#' In the second part of this training, we're going to be 
#' talking about using additional packages and reading data
#' into R. 
#' 
#' Everything we have been doing so far has been in what is
#' called "Base R", that is, the functions and packages that
#' come with R and are loaded up every time R is opened. 
#' There's nothing wrong with base R, but it can often be
#' quite hard to remember all of the functions and, as 
#' the things you want to do get more complex, can often
#' mean that the code becomes harder and harder to read and
#' understand.
#' 
#' For the rest of this training, we're going to be using
#' some additional packages called the tidyverse, janitor,
#' haven, and labelled.
#' 
#' tidyverse: is a collection of packages that follow the 
#' same design philosophy in terms of arguments, function
#' names, and interoperability. They work in such a way that
#' the code you write tends to be very readable, and is 
#' often read top to bottom and left to right rather than
#' from the innermost function to the outermost.
#' 
#' haven: is a package for reading in all kinds of different
#' data files, such as SPSS .sav files or STATA .dat files.
#' 
#' janitor: is a package with several helpful functions for
#' creating tables and formatting them,
#' 
#' labelled: is a package that is especially useful for 
#' working with SPSS data files, in order to strip them of
#' all the excess SPSS features that 99 times out of 100,
#' we don't need.
#' 

# 9. But what is a package anyway? ---------------------

#' A package is effectively a collection of functions and 
#' other resources that other people have created that you
#' can use to extend the functionality of R and RStudio. If
#' there is something you want to do in R, someone has 
#' probably made a package to do it (or to make it easier
#' to do it). There are packages that add modeling 
#' functionality, packages that scrape data, packages that
#' create websites, and on and on.
#' 
#' Before you can use a package, you need to download it 
#' using the install.packages() function. This requires you
#' to be online on your computer. Inside the 
#' install.packages() function, you need to type the name
#' of the package in "" quotation marks. For example, if
#' you want to install the four packages I mentioned above,
#' you can run the following lines of code:

install.packages("tidyverse")
install.packages("janitor")
install.packages("haven")
install.packages("labelled")

#' However, even after you've downloaded a package doesn't
#' mean that you can use it. First, you have to load it from
#' your library of packages using the library() function. 
#' Why? Because otherwise you might end up having all of your
#' packages loaded all of the time, and many of these packages
#' will probably use the same names for functions. Because
#' of this, R makes you load your packages from the library
#' **every time you start up R/RStudio**

library(tidyverse)
library(janitor)
library(haven)
library(labelled)

#' I try to think of install.packages() as if you are going
#' to the bookshop to buy a book that you can use in your
#' work. You only need to do this once. You take that book
#' home, and then put it in your library, on your bookcase.
#' Then, when you want to use the book, you go to your 
#' library() and take the book down from the bookcase and
#' put it on your desk to use it. After you finish working,
#' (because you're such a tidy person), you put the book
#' back on the bookcase. The next time you go to start work,
#' you need to go back and collect it from the library. Of
#' course, you would never want to put all of the books 
#' that are in your library on your desk at the same time,
#' as you wouldn't have any space left to work.


# 10. Reading data from your computer ----------------

#' Pretty much 100% of the time, you're going to want to
#' be using data that's saved to your own computer to do
#' data analysis. To get data "into" R, you need to "read"
#' it. The most common data formats to use in R are .csv
#' files, but it can handle any data formats and even has 
#' its own .RDS format.
#' 
#' Probably the biggest learning curve in R is getting R
#' to successfully find the data on your computer. When 
#' you ask R to read data, it looks inside what is called
#' the "working directory". If you set up, or opened your
#' RStudio using a project (blue box), then your working 
#' directory will be set to the location of that project.
#' 
#' I ALWAYS RECOMMEND SETTING UP A PROJECT WHEN USING R.
#' 
#' If you are running this RStudio session from the R 
#' project that was set up with it, then the following two
#' lines of code should read two different datasets for 
#' you:

bigmac <- read_csv(file = "data/bigmac.csv")
work <- read_spss(file = "data/workprog.sav")

bigmac
work

#' If this has successfully worked, i.e. you didn't get a 
#' "connection" error saying there is no such file or 
#' folder, then your working directory is set to the 
#' correct place. You can find out *where* R is looking 
#' for files by running the following code:

getwd()

#' So, in this case, when I use read_csv() with the file 
#' argument set to "data/bigmac.csv", this means that R is
#' looking inside the pgrt-intro-data-analysis-R folder,
#' inside a folder called data, for a file called 
#' bigmac.csv.

#' We now have two new datasets to use. But first, let's
#' tidy up the SPSS data file and get some more information
#' about it.

# 11. Tidying SPSS data files ----------------

#' If you've ever used SPSS before, you'll probably know that
#' it works by attaching value labels to specific numeric
#' values which represent them (for example, in the data
#' we loaded earlier the numeric value 0 in the marital
#' variable means Unmarried, while the numeric value 1 in
#' that variable means Married). R doesn't really need 
#' this kind of "number plus label" setup, as it knows how
#' to handle different variable types. It would be 
#' preferable for us to just have the regular text (character
#' strings) in our dataset, preferably with the same 
#' organisation. We also probably want any missing data to
#' just be read as R as missing (SPSS allows you to set
#' several different types of missing variable).
#' 
#' We can use the labelled package to do this. However,
#' before we do, let's talk about switching our R code
#' from reading from the inside to outside, for example,
#' like this:

unlabelled(user_na_to_na(work))

#' To reading like this:

work %>%
  user_na_to_na() %>%
  unlabelled()

#' Both of these pieces of code do the exact same thing, 
#' but for the first style, we have to read the code from the
#' innermost brackets to the outermost, as well as from
#' left to right. This quickly becomes difficult, especially
#' if multiple arguments are involved. Consider the following
#' extreme examples:

summarise(group_by(filter(work, marital == 1), gender), mean_age = mean(age))

#' Without running the code, can you figure out what the
#' above code is supposed to do?
#' 
#' How about the below, identical code:

work %>%
  filter(marital == 1) %>%
  group_by(gender) %>%
  summarise(
    mean_age = mean(age)
  )

#' The above symbol, %>%, can be read as "and then". For 
#' example, "Take the work dataset, and then, filter all
#' of the rows where marital status is equal to 1, and then,
#' group the rows by gender, and then, summarise the mean
#' of age as a variable called mean_age.
#' 
#' Anyway, let's go back and save our tidied SPSS data

work <- work %>%
  user_na_to_na() %>%
  unlabelled()


#' We might also want to take a look at our variable labels
#' in the same way we might in spss using labelled's var_label
#' function:

var_label(work)


# 12. Creating a new variable, revisited --------------

#' Let's create another new variable in our big_mac data
#' which shows the price in US dollars by dividing the
#' local price by the exchange rate. Instead of using the
#' dollar syntax (which is often an easy way to introduce
#' errors), we're going to instead use the "mutate" 
#' function.
#' 
#' First, I can test whether the newly created variable
#' works:

bigmac %>%
  mutate(
    price_dollars = local_price / dollar_ex
  ) 

#' and then run the same code but save the result:

bigmac <- bigmac %>%
  mutate(
    price_dollars = local_price / dollar_ex
  ) 


#' We'll use this later when we revisit visualisation.


# 13. Recoding variables using case_when --------------

#' Another common task in data analysis is *recoding*,
#' especially combining categories of ordinal or
#' categorical variables together. For example, let's say
#' we want to take our education (ed) variable from our
#' work dataset, and then create a new, recoded variable,
#' where we just have whether someone has no formal education
#' or whether they have at least high school level education.
#' 
#' We can use the function case_when within a mutate() 
#' function in order to achieve this. I will usually also
#' bring up a table of the variable to help me with writing
#' this code:

table(work$ed)

work <- work %>%
  mutate(
    ed2 = case_when(is.na(ed) ~ NA_character_,
                    ed == "Did not complete high school" ~ "No qualifications",
                    TRUE ~ "At least high school qualifications"
                    )
  )

#' Let's break down this case_when() function a bit, as it's
#' not so clear what's going on. The first line, is.na(ed) ~
#' NA_character_, is saying "if there is a missing value in 
#' the ed variable, then we want that value to be missing in 
#' the new ed2 variable we are creating. The second line,
#' ed == "Did not complete high school" ~ "No qualifications",
#' is saying "if the value for ed is "Did not complete 
#' high school", then we want the value in ed2 to be "No 
#' qualifications. Lastly, the final like, which reads,
#' TRUE ~ "At least high school qualifications", means
#' "if there is any other value in the variable, then make 
#' it become "At least high school qualifications" in the 
#' ed2 variable". 
#' 
#' You can have as many lines and conditions in your case_when
#' argument. 
#' 
#' Let's check whether it worked by running a table on the
#' new ed2 variable we created. 

table(work$ed2)

# 14. Descriptive statistics, revisited --------------

#' Okay, now let's revisit some of our descriptive 
#' statistics from earlier. You already saw in the example
#' of the very long code how I used the summarise()
#' function to calculate the mean() for a specific 
#' variable. Let's repeat that example to get the 
#' average Big Mac price:

bigmac %>%
  summarise(
    mean_price = mean(price_dollars, na.rm = TRUE)
  )

#' However, we can also extend this code to do more,
#' for example, let's imagine we wanted to filter our
#' data so that it only included values that were from
#' January 1st 2022 and onwards:

bigmac %>%
  filter(date > date("2022-01-01")) %>%
  summarise(
    mean_price = mean(price_dollars, na.rm = TRUE)
  )

#' Or what if we wanted to just select the United Kingdom
#' and the United States and then group the means by 
#' country?

bigmac %>%
  filter(name %in% c("Britain", "United States")) %>%
  group_by(name) %>%
  summarise(
    mean_price = mean(price_dollars, na.rm = TRUE)
  )

#' How about tables? One of the packages that we downloaded
#' earlier was called janitor, and it has some neat functions
#' for making and formatting tables. Rather than "table", janitor
#' differentiates its tables by calling them "tabyl"s. 
#' Let's make a table of the education data in our work 
#' dataset:

work %>%
  tabyl(ed)

#' Let's repeat our contingency table with the chickwts
#' dataset

chickwts %>%
  tabyl(big, feed)

#' This looks pretty similar to what we had before. However
#' we can add additional things to the tabyl in a way that
#' we couldn't with a table. For example, we can calculate
#' percentages and add percentage formatting:

chickwts %>%
  tabyl(big, feed) %>%
  adorn_percentages(denominator = "row") %>%
  adorn_pct_formatting()

#' You can even do a chi squared test directly from a tabyl:

chickwts %>%
  tabyl(big, feed) %>%
  chisq.test()


# 15. Visualisation, revisited -----------------------

#' Something I mentioned in one of the previous sections
#' was that making plots in base R quickly becomes quite
#' difficult: for a super quick and simple plot, it's very
#' easy to use base R. But as soon as you want to do more
#' customisation, overlay multiple different aesthetics, 
#' and so on, adjust titles and axes, it becomes very 
#' difficult.
#' 
#' ggplot2 is a package which is a part of the tidyverse
#' and can be used for making highly customisable plots.
#' For example, let's take our plots from earlier, such
#' as the histogram of weights of chicks:

chickwts %>%
  ggplot() +
  geom_histogram(
    aes(x = weight)
  )

#' Well, you might say, that looks like a lot more code 
#' for basically the same result! However, this is where
#' the magic of ggplot2 comes in. Even though it takes a lot
#' more code initially, it becomes much, much, easier to
#' make more complex adjustments. For example, let's say we 
#' wanted to fill our histogram bars in according to the
#' value of some other variable, like the cutoff for big; we
#' can just add an additional aesthetic, "fill", into our
#' aes() function:

chickwts %>%
  ggplot() +
  geom_histogram(
    aes(x = weight, fill = big)
  )

#' But let's rewind a bit and go through what this code 
#' is doing. 
#' 
#' The first part of the code is the data, we are taking 
#' the data, and then, starting ggplot(). I describe the 
#' ggplot() function a bit like starting a car: we've 
#' told R we're going to plot something, but we've not
#' told it what that plot is going to look like yet.
#' 
#' The next function, geom_histogram, tells R what kind
#' of visualisation of the data that we want. In this case,
#' it's fairly easy to guess what geom_histogram means,
#' but other geoms can be more cryptic.
#' 
#' aes() is probably the most difficult function to wrap
#' ones head around when it comes to ggplot2. In very simple
#' terms, everything that goes in aes() is something that you
#' want to vary according to a variable in your dataset;
#' for example, this might be the position on the x or 
#' y axis, or, as in our above example, the colour that
#' each bar is filled in. But you can add arguments outside
#' of the aes() function for **fixed** aesthetics; that is,
#' things that don't vary according to a variable in our
#' dataset. For example, look what happens when we move
#' our fill argument outside the aes function and make it
#' "blue":

chickwts %>%
  ggplot() +
  geom_histogram(
    aes(x = weight), fill = "blue"
  )

#' Now everything is blue, there is no variation in the fill
#' of the histogram.
#' 
#' Let's try a few more examples, each with a small tweak
#' to them. First, our line plot from earlier, bit with points
#' overlayed and then coloured in according to the Chick ID:

ChickWeight %>%
  ggplot() +
  geom_line(aes(x = Time, y = weight, group = Chick)) +
  geom_point(aes(x = Time, y = weight, colour = Chick)) +
  theme(
    legend.position = "none"
  )


#' You can also create a boxplot in ggplot2, and something
#' that is particularly nice is that you can make it a 
#' horizontal box plot if you like (which is handy for
#' reading labels):

work %>%
  ggplot() +
  geom_boxplot(aes(x = incaft, y = ed)) +
  xlab("Income After Intervention") +
  ylab("Level of Education")


#' You can also create bar plots without having to first
#' calculate the table if you use geom_bar in either 
#' horizontal or vertical orientation:

work %>%
  ggplot() +
  geom_bar(
    aes(x = ed)
  )

work %>%
  ggplot() +
  geom_bar(
    aes(y = ed)
  )



# 16. Trying it out yourself ------------------------

#' We've covered quite a lot today so well done for making 
#' it through! Remember that coding and R is for everyone;
#' you don't need to be some kind of "person who is good
#' with computers" or "person who is good at maths" or logic
#' to be able to make use of R. All you need is the willing-
#' ness to try something that feels a little bit different
#' from the way we use most things on computers these days.
#' 
#' Running code in R is no different to clicking menus and 
#' icons to make your computer do something; the only 
#' difference is you're typing what you want it to do instead
#' of clicking. Getting an error or a warning is fine: nothing
#' bad is going to happen to your computer. Think of every 
#' time you have had to press Undo or Back while using
#' your computer or phone, that's no different to making an
#' error and then having to try again. The only difference 
#' is what it looks like.
#' 
#' If you're feeling inspired and you'd like to try more,
#' I'd encourage you to use the space below to experiment.
#' For example, could you try making a scatterplot of the
#' income before (incbef) and the income after (incaft) 
#' variables in work using either plot() or ggplot with 
#' geom_point? Could you try doing an ANOVA test to find
#' out whether people who were in the programme group 
#' (prog) made significantly more money after the 
#' intervention? Below this section are some recommended
#' resources for taking your R learning further.





# 17. Resources for learning R -----------------------

#' Wickham, H., Cetinkaya-Rundel, M., and Grolemund G. 
#' (2022). R for Data Science. O'Reilly. Available free
#' online: https://r4ds.hadley.nz 
#' 
#' Mehmetoglu, M. & Mittner, M. (2021). Applied Statistics
#' Using R: A Guide for the Social Sciences. Sage.
#' https://uk.sagepub.com/en-gb/eur/applied-statistics-using-r/book266647
#' 
#' Estrellado, et al. (2020). Data Science in Education 
#' Using R. Routledge. Available free online:
#' https://datascienceineducation.com/ 
#' 
#' Navarro, D. (2015) Learning Statistics with R. 
#' Available free online:
#' https://learningstatisticswithr.com/

