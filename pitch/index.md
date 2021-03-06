---
title       : Classifying your mails
subtitle    : Detecting whether your mails are spam or not
author      : Sven Rogge
job         : 
framework   : io2012   # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js  # {highlight.js, prettify, highlight}
hitheme     : zenburn     # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : selfcontained # {standalone, draft}
knit        : slidify::knit2slides
---

<style>
em {
  font-style: italic
}
</style>

## Purpose of the Shiny app


- Are you tired of receiving numerous spam mails?

- Don't you trust the all-purpose spam filters?

- Do you think you can do better?

- Then try our [our new Shiny app](http://svenrogge.shinyapps.io/MailClassification):

    * Easy to use
    
    * Easy to adapt
    
    * Easy to interpret
    
    * Easy to generalize
    
- Let Bill Gates convince you:

    _Like almost everyone who uses e-mail, I receive a ton of spam every day. 
    Much of it offers to help me get out of debt or get rich quick. It would be funny 
    if it weren't so exciting._

--- .class #id 

## Building the prediction

- Our spam filter relies on the `spam` database, provided in the *ElemStatLearn* library of RStudio. This dataset contains 57 variables, including the frequency with which certain words and characters appear, and the presence of capital letters, and states whether a mail is spam or not. More information on the dataset can be found at the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Spambase). 




```r
data(spam); paste(ncol(spam),nrow(spam))
```

```
## [1] "58 4601"
```

- From this data set, a linear model is built, determining the type of mail as function of these 57 variables.

- Your mail is screened for the 57 spam indicators, and its spam score is predicted using the previously built model.

---

## Conclusions and expansions

- Since the prediction model was built on a case-specific data set, it will work best when analyzing similar mails. For instance, the area code of the HP headquarters, where the data set was established, is an indicator for non-spam.

- However, the idea behind this app can be generalized for other applications, once a reference data set is given. One only has to build a new linear model, and adapt the function `makeDF`, which converts the input mail to a data frame only containing relevant information. The source code can be found on this GitHub repository.

- Try it yourself on the next slide!

---

<iframe width="640" height="360" src="http://svenrogge.shinyapps.io/MailClassification" frameborder="0" allowfullscreen></iframe>
