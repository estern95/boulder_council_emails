# Boulder Colorado Local Politics

In the current world of 2020, local politics are more important than ever. Unfortunately, with the continual degredation of local new sources and the increase attention of national politics, a lot of important local measure get ignored. This is a very unfortunate trend. As it means that those who have the local politicians ears are not always a representative sample of the population. 

## City Council Mailbox

I currently live in Boulder, Colorado working as a data scientist in clean energy. Boulder has a very eclectic population leading to a lot of different and sometimes perplexing opinions regarding local politics. One view into this world is through the publicly availible city council mailbox. All emails sent to and from this address are anonymized and stored nightly to a csv file by year. The goal of this project is to A) Understand how local constituents interact with their politicians, and B) Organize this data in a way that make accessing it easier.

## Methods

To help understand how constituents are interacting with their politicians, I used some standard text clustering using Latent Dirchlet Allocation to create topic bins. After this, I am currently building a dashboard to showcase these bins and a viewer using Shiny. For more details on these methods, please check out the R notebook, [explore_council_emails.rmd](https://github.com/estern95/boulder_council_emails/blob/master/explore%20council%20emails.rmd). 
