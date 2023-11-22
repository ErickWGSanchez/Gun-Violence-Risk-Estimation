library(micromapST)


# Example # 01 - map with no cumulative shading,
#     2 columns of statistics: dot with 95% confidence interval, boxplot
#     sorted in descending order by state rates, using the default
#     border group of "USStatesBG", with default symbols.

# load sample data, compute boxplot

data(wflung00and95,wflung00and95US,wflung00cnty,envir=environment()) 

wfboxlist = boxplot(split(wflung00cnty$rate,wflung00cnty$stabr),plot=FALSE)  

# set up 4 column page layout

panelDesc01 <- data.frame(
  type=c("map","id","dotconf","boxplot"),    
  lab1=c(NA,NA,"State Rate","County Rates"),  
  lab2=c(NA,NA,"and 95% CI","(suppressed if 1-9 deaths)"), 
  lab3=c(NA,NA,"Deaths per 100,000","Deaths per 100,000"), 
  col1=c(NA,NA,1,NA),col2=c(NA,NA,3,NA),col3=c(NA,NA,4,NA),     
  refVals=c(NA,NA,wflung00and95US[1,1],NA),   
  refTexts=c(NA,NA,"US Rate 2000-4",NA),       
  panelData= c(NA,NA,NA,"wfboxlist")          
) 
# set up PDF output file, call package

pdf(file="Ex01-WFLung-2000-2004-State-Dot-County-Box.pdf",width=7.5,height=10)
#or windows() if you want to print it in an R window

micromapST(wflung00and95, panelDesc01, sortVar=1, ascend=FALSE,
           title=c("Ex01-White Female Lung Cancer Mortality, 2000-2004", 
                   "State Rates & County Boxplots")
)  

dev.off()


#____________________________________

# Example # 02 - map with cumulative shading 
#     from top down (mapcum), arrow and bar charts, 
#     sorted in descending order by starting
#     value of arrows (1950-69 rates) using default
#     border group of "USStatesDF".  This 
#     example also provides custom colors for the 
#     linked micromaps, highlights, etc.
#   

# Load example data from package.

data(wmlung5070,wmlung5070US,envir=environment())  

panelDesc02 <- data.frame(
  type=c("mapcum","id","arrow","bar"),		
  lab1=c(NA,NA,"Rates in","Percent Change"),       
  lab2=c(NA,NA,"1950-69 and 1970-94","1950-69 To 1970-94"),  
  lab3=c("MAPCUM",NA,"Deaths per 100,000","Percent"),
  col1=c(NA,NA,"RATEWM_50","PERCENT"), 		
  col2=c(NA,NA,"RATEWM_70",NA)		
)

colorsRgb = matrix(c(                    # the basic 7 colors.
  213,  62,  79,   #region 1: red	   
  252, 141,  89,   #region 2: orange	    
  253, 225, 139,   #region 3: green	    
  153, 213, 148,   #region 4: greenish blue 
  50, 136, 189,   #region 5: lavendar 	    
  255,   0, 255,   #region 6                    
  .00, .00, .00,   #region 7: black for median 
  230, 245, 152,   #non-highlighted foreground 
  255, 174, 185,   # alternate shape upper   
  191, 239, 255,   # alternate shape lower  
  242, 242, 242,   # lightest grey for non-referenced sub-areas 
  234, 234, 234),  # lighter grey for background 
  
  ncol=3,byrow=TRUE)

xcolors = c( grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],
                            maxColorValue=255),
             # set solid colors
             grDevices::rgb(colorsRgb[,1],colorsRgb[,2],colorsRgb[,3],64,
                            maxColorValue=255))   
# set translucent colors for time series.

# set up reference names for color set
names(xcolors) =c("rustred","orange","lightbrown","mediumgreen", 
                  "blue","magenta", "black","yellowgreen",
                  "mauve","cyan","lightest grey","lighter grey",
                  "l_rustred","l_orange","vlightbrown","lightgreen", 
                  "lightblue","l_black","l_yelgreen","l_mauve",
                  "l_cyan","l_lightest grey","l_lighter grey")       

pdf(file="Ex02-WmLung50-70-Arrow-Bar.pdf",width=7.5,height=10)

#or windows() if you want to print it in an R window
micromapST(wmlung5070,panelDesc02,sortVar=1,ascend=FALSE,
           title=c("Ex02-Change in White Male Lung Cancer Mortality Rates",
                   "from 1950-69 to 1970-94-Diff colors"), colors=xcolors
) 

dev.off()

#____________________________________


# Example # 03 - Time Series Line Plots with Confidence Bands
#     maptail option highlights states from extremes to middle state
#     read in time series data set example using the default border group of "USStatesDF".


data(TSdata,envir=environment())  
temprates     <-  data.frame(TSdata[,,2])  

# TSdata structure is array of size c(51,15,4), 
# dimensions = 51 states, 15 years, (year label, point value, low limit, high limit)

panelDesc03   <- data.frame(                    
  type=c("maptail","id","tsconf","dot"),      
  lab1=c(NA,NA,"Time Series","Female"),  
  lab2=c(NA,NA,"Annual Rate per 100,000","Most Recent Rate (2010)"),  
  lab3=c(NA,NA,"Years","Deaths per 100,000"), 
  lab4=c(NA,NA,"Rate",NA),		  
  col1=c(NA,NA,NA,15),        
  panelData =c(NA,NA,"TSdata",NA)
)
ExTitle       <- c("Ex03-Time Series with Confidence bands",
                   "Annual Female Lung Cancer Mortality Rates, 1996-2010")

pdf(file="C:/Users/justd/Desktop/Ex03-Time-Series-with-Conf.pdf",width=7.5,height=10)

micromapST(temprates,panelDesc03,sortVar="P15",ascend=FALSE,title=ExTitle)  

dev.off()

# __________________________________


# Example 04 - dot followed by a scatter dot columns
#     use same data as Example 3 to compare 1996 & 2010 rates
#     mapmedian option shades states above or below the median (light yellow)
#     using the default border group of "USStatesBG"
#
#   USES data loaded for Example 03 (temprates).
#

panelDesc04 <- data.frame(                 
  type=c("mapmedian","id","dot","scatdot"),  
  lab1=c(NA,NA,"Female Lung Cancer Mortality","Comparison of Rates"),   
  lab2=c(NA,NA,"Rate in 1996 (Sort Variable)",
         "in 1996 (x axis) and 2010 (y axis)"),   
  lab3=c(NA,NA,"Deaths per 100,000","Deaths per 100,000 in 1996"), 
  lab4=c(NA,NA,NA,"Rate in 2010"),	
  col1=c(NA,NA,1,1),                 
  col2=c(NA,NA,NA,15)		
)
ExTitle <- c( "Ex04-Dot Plot for 1996, Scatter Plot Comparing 1996 to 2010",
              "Female Lung Cancer Mortality Rates")

pdf(file="Ex04-Scatter-Dots.pdf",width=7.5,height=10)
micromapST(temprates,panelDesc04,sortVar=1,ascend=FALSE,title=ExTitle)  

dev.off()

#_____________________________________

# Example 05 - horizontal stacked (segmented) bars
#     segbar plots the input data, normbar plots percent of total
#     package computes the percents from input data
#     input for the categories for each state must be in consecutive 
#     columns of the input data.frame using the default border group of "USStatesBG"

data(statePop2010,envir=environment())

panelDesc05 <- data.frame(                   
  type=c("map","id","segbar","normbar"), 
  lab1=c(NA,NA,"Stacked Bar","Normalized Stacked Bar"), 
  lab2=c(NA,NA,"Counts","Percent"),     
  col1=c(NA,NA,"Hisp","Hisp"),                     
  col2=c(NA,NA,"OtherWBH","OtherWBH")		  
)

pdf(file="Ex05-Stkd-Bar-var-height.pdf",width=7.5,height=10)
micromapST(statePop2010, panelDesc05, sortVar="OtherWBH", ascend=FALSE,
           title=c("Ex05-Stacked Bars: 2010 Census Pop by Race, Sorted by Count Other Race",
                   "Cat-L to R: Hispanic, non-Hisp White, Black, Other-sn-varbar"),
           details=list(SNBar.varht=TRUE), axisScale="sn" )  
dev.off()
