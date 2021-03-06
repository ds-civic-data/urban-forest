Project Proposal
================
Mitzi Zitler, Frank Gaunt, Clark Chang

Themes
------

Trees! We're going to look at trees and what effect they have on house prices, using the literature as a guide. We will expand our analysis to include census data, examining demographics in portland in relation to the trees.

Questions
---------

Do trees affect housing prices in neighborhoods? How does this vary by neighborhood? How can we best visualize this? How does this interact with census data, with demographics and household sizes.

Relevant Work
-------------

Relevant work includes the following literature:

"Valuing green infrastructure in Portland, Oregon" <https://www.sciencedirect.com/science/article/pii/S0169204614000036>

"Trees in the city: valuing street trees in Portland, Oregon" <https://www.fs.usda.gov/treesearch/pubs/36186>

"The Effect of Trees on Crime in Portland, Oregon" <http://journals.sagepub.com/doi/abs/10.1177/0013916510383238>

Clients
-------

Potential clients include Angie DiSavlo, Nik Desai, Ryan Kinsella, and Jeff Ramsay.

Data
----

### Sources

On the tree side, data will mostly come from the USDA forestry data, as well as some Portland neighborhood guidelines and tree rules.

City of Portland - Parks & Recreation: <https://www.portlandoregon.gov/parks/article/433143>

On the housing price side, data will primarily come from zillow and redfin's APIs.

Some other potential sources iclude Portland Maps Metadata for tracking down any number of neighborhood characteristics, as well as potentially census data to get demographic information.

### Data structure

Observational unit

The observational unit will likely be the neighborhoods of Portland, as there are many neighborhoods with different tree distributions and tree types, as well as well documented demographic information.

Vairables

Variables will definitely include trees, might also include categorical like number of major roadways, or population estimates, as well as, of course: house prices.

Confidentiality

Certain demographic information might be so specific that we might run into the issue of just revealing where minorities live. This is extremely sensitive. In other cases of data confidentiality, edge cases in religion and ethnicity were excluded from the data set in fear of revealing sensitive information.

Deliverable
-----------

We will be making a Shiny App, which will visualize spatially the distribution of trees across Portland metro by tree type, and this can be overlaid with various demographic information, and infrastructural information like bike paths, major roadways, etc.
