## Overview
Word Predictor was my capstone project for the John Hopkins University Data Science Specialization course. It takes the beginning of a sentence or sentence fragment and makes a prediction on the most likely next word.

### How It Works
- Samples were gathered from a variety of sources found on the internet, including blogs, news articles, and Twitter. (Source data can be found here: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip)
- This equated to roughly 1.28 million samples comprising 2.44 million sentences.
- From these sentences, various forms of n-grams and skip-grams were constructed for evaluation, from 1-grams up to 5-grams.
- The n-grams were then compiled and filtered to remove any n-gram that appears only once. (A single appearance of an n-gram does not have much predictive value and also greatly bloats the size of our model.)
- For each n-gram, the related (n-1)-gram was compiled to find the highest occuring last word.
    - For example, if we have a 5-gram "I'm going to the mall" that occurs 5 times, and another 5-gram "I'm going to the beach" that occurs 7 times, the highest occurring last word would be "beach".
- These highest occurring n-gram pairings were then added to lookup tables and used for the word prediction algorithm.
- Of course, the n-grams won't fit exactly with all word inputs, so the "best fit" is found in the lookup tables using a "backoff" method:
    - Try to fit the last 4 words, and if no match is found, "backoff" and try to match the last 3 words and so forth.
    - Skip-grams are also used to increase versatility.
    - The backoff model will go in order of precedence of the greatest number of words matching within closest proximity to the predicted word. The full order of precedence is outlined below.

### Order Of Precedence
(I will use our example of "I'm going to the" to illustrate the construction of our n-grams.)
- 4-gram ("I'm going to the")
- 3-gram ("_ going to the")
- 4-gram skipping 1 in position 2 ("I'm _ to the")
- 4-gram skipping 1 in position 3 ("I'm going _ the")
- 4-gram skipping 1 in position 4 ("I'm going to _")
- 2-gram ("_ _ to the")
- 3-gram skipping 1 in position 2 ("_ going _ the")
- 3-gram skipping 1 in position 3 ("_ going to _")
- 4-gram skipping 2 in position 2 and 3 ("I'm _ _ the")
- 4-gram skipping 2 in position 2 and 4 ("I'm _ to _")
- 4-gram skipping 2 in position 3 and 4 ("I'm going _ _")
- 1-gram  ("_ _ _ the")
- 2-gram skipping 1 ("_ _ to _")
- 3-gram skipping 2 ("_ going _ _")
- 4-gram skipping 3 ("I'm _ _ _")

### Try It Out
- The Word Predictor application is available at: https://nomadmonday.shinyapps.io/WordPredictor/
- Be a little patient at first. According to the server logs, it takes approximately 17.7 seconds for the application to initialize.
- However, once initialized, the actual prediction is very fast. Locally tested using the system.time() function, the mean user time was just 0.0002 seconds. (Total user time to run over a test set of 167,619 sentence fragments was 28.17 seconds.)

## File Descriptions
