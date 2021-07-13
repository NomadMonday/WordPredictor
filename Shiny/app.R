library(shiny)
library(shinyjs)
library(readr)
library(dplyr)
library(quanteda)
library(data.table)


# Define UI
# The main content is initially hidden and a loading display is shown first.
# After the server logic is loaded, the main content will be unhidden.
ui <- fluidPage(
	useShinyjs(),
	hidden(div(
		id = "main_content",
		
		# Application title
		titlePanel("Word Predictor!"),
		h5('Given a partial sentence input, Word Predictor! will predict the next likely word.'),
		
		# Layout
		sidebarLayout(
			sidebarPanel(
				textInput("inputValue", "Input text:")
			),
			
			mainPanel(
				h4("Prediction:"),
				textOutput("outputValue")
			)
		)
	)),
	# Loading display
	div(
		id = "loading_page",
		h2("Loading, please wait..")
	)
)

# Define server logic
server <- function(input, output, session) {
	#Create lookup tables.
	lookupBigram <- read_csv("./lookup_tables/lookupBigram.csv") %>% as.data.table() %>% setkey(key)
	lookupBigram_s1 <- read_csv("./lookup_tables/lookupBigram_s1.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram <- read_csv("./lookup_tables/lookupQuadgram.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s1p2 <- read_csv("./lookup_tables/lookupQuadgram_s1p2.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s1p3 <- read_csv("./lookup_tables/lookupQuadgram_s1p3.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s1p4 <- read_csv("./lookup_tables/lookupQuadgram_s1p4.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s2p23 <- read_csv("./lookup_tables/lookupQuadgram_s2p23.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s2p24 <- read_csv("./lookup_tables/lookupQuadgram_s2p24.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s2p34 <- read_csv("./lookup_tables/lookupQuadgram_s2p34.csv") %>% as.data.table() %>% setkey(key)
	lookupQuadgram_s3 <- read_csv("./lookup_tables/lookupQuadgram_s3.csv") %>% as.data.table() %>% setkey(key)
	lookupTrigram <- read_csv("./lookup_tables/lookupTrigram.csv") %>% as.data.table() %>% setkey(key)
	lookupTrigram_s1p2 <- read_csv("./lookup_tables/lookupTrigram_s1p2.csv") %>% as.data.table() %>% setkey(key)
	lookupTrigram_s1p3 <- read_csv("./lookup_tables/lookupTrigram_s1p3.csv") %>% as.data.table() %>% setkey(key)
	lookupTrigram_s2 <- read_csv("./lookup_tables/lookupTrigram_s2.csv") %>% as.data.table() %>% setkey(key)
	lookupUnigram <- read_csv("./lookup_tables/lookupUnigram.csv") %>% as.data.table() %>% setkey(key)
	
	lookupDefault <- read_lines("./lookup_tables/lookupDefault.txt")
	
	#Create prediction function.
	#Take at most the last 4 words and match it to the n-gram via lookup tables.
	#Lookup tables used in order of precedence of the most words in closest proximity to the end.
	predictWord <- function(s)
	{
		#Return empty if s is empty.
		if(nchar(s) == 0)
		{
			return("")
		}
		
		#Tokenize input through quanteda in order to match formatting to lookup tables.
		x <- s %>% corpus() %>% corpus_reshape("sentence") %>% tail(1) %>% tokens(remove_punct = TRUE)
		x <- x[[1]] %>% tail(4)
		
		#Progress through lookup tables for a match.
		#Last 4 words.
		if(length(x) == 4)
		{
			key <- x %>% paste(collapse = "_")
			value <- lookupQuadgram[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#Last 3 words.
		if(length(x) >= 3)
		{
			key <- x %>% tail(3) %>% paste(collapse = "_")
			value <- lookupTrigram[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#3 words skipping 1.
		if(length(x) == 4)
		{
			#Skip 1 position 2.
			key <- c(x[1], x[3], x[4]) %>% paste(collapse = "_")
			value <- lookupQuadgram_s1p2[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
			
			#Skip 1 position 3.
			key <- c(x[1], x[2], x[4]) %>% paste(collapse = "_")
			value <- lookupQuadgram_s1p3[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
			
			#Skip 1 position 4.
			key <- c(x[1], x[2], x[3]) %>% paste(collapse = "_")
			value <- lookupQuadgram_s1p4[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#Last 2 words.
		if(length(x) >= 2)
		{
			key <- x %>% tail(2) %>% paste(collapse = "_")
			value <- lookupBigram[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#2 words skipping 1.
		if(length(x) >= 3)
		{
			#Skip 1 position 2.
			key <- x %>% tail(3)
			key <- c(key[1], key[3]) %>% paste(collapse = "_")
			value <- lookupTrigram_s1p2[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
			
			#Skip 1 position 3.
			key <- x %>% tail(3)
			key <- c(key[1], key[2]) %>% paste(collapse = "_")
			value <- lookupTrigram_s1p3[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#2 words skipping 2.
		if(length(x) == 4)
		{
			#Skip 2 position 2+3.
			key <- c(x[1], x[4]) %>% paste(collapse = "_")
			value <- lookupQuadgram_s2p23[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
			
			#Skip 2 position 2+4.
			key <- c(x[1], x[3]) %>% paste(collapse = "_")
			value <- lookupQuadgram_s2p24[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
			
			#Skip 2 position 3+4.
			key <- c(x[1], x[2]) %>% paste(collapse = "_")
			value <- lookupQuadgram_s2p34[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#Last 1 word.
		if(length(x) >= 1)
		{
			key <- x %>% tail(1)
			value <- lookupUnigram[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#Skipping 1.
		if(length(x) >= 2)
		{
			key <- x %>% tail(2)
			key <- key[1]
			value <- lookupBigram_s1[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#Skipping 2.
		if(length(x) >= 3)
		{
			key <- x %>% tail(3)
			key <- key[1]
			value <- lookupTrigram_s2[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#Skipping 3.
		if(length(x) == 4)
		{
			key <- x[1]
			value <- lookupQuadgram_s3[key, 2] %>% as.character()
			if(!is.na(value))
			{
				return(value)
			}
		}
		
		#No matches: return default.
		return(lookupDefault)
	}
	
	#Initiation complete. Hide loading page and unhide the main content.
	shinyjs::hide("loading_page")
	shinyjs::show("main_content")	
	
	output$outputValue = renderText(
	{
		predictWord(input$inputValue)
	})
}

# Run the application 
shinyApp(ui = ui, server = server)

