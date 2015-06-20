library(shiny)
shinyUI(pageWithSidebar(
 headerPanel('Classify your mail as spam/non-spam'),
 sidebarPanel(
   h3('Getting started'),
   p("This app predicts the probability of your mail to be spam. To use this app,
     paste your mail in the text box to the right, and click 'Start Prediction' below. 
     This app will then process your mail, select the relevant identifiers (see the
     documentation on the Hewlett-Packard spam database), and predict the probability
     of being spam based on a linear regression model. Note that the database used to
     train the linear model is specific to the HP classification problem, and can yield
     surprising results on other mails.", align='justify'),
   h3('Ready to try it out?'),
   p("If you're ready, please click the 'Start Prediction' button below."),
   submitButton('Start Prediction')
 ),
 mainPanel(
   h3('Input section'),
   p('Please insert your mail in the textbox below.'),
   tags$textarea('Paste your mail here', id = 'id1', placeholder = 'Your mail.', rows = 8, ""),
   tags$style(type='text/css', "#id1 { height: 200px; width: 400px; }"),
   h3('Output section'),
   p('Please find below the probability of your mail being spam.'),
   verbatimTextOutput('oid1')
 )
))