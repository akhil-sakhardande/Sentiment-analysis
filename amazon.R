#####################################################################
# Function for downloading product reviews from amazon.in
# Usage:
# url = "http://www.amazon.com/GoPro-CHDHY-401-HERO4-SILVER/product-reviews/B00NIYJF6U/ref=cm_cr_pr_btm_link_2?ie=UTF8&showViewpoints=1&sortBy=recent&reviewerType=all_reviews&formatType=all_formats&filterByStar=all_stars&pageNumber=2"
# pr_review = amazon.com(url,10)
#
# url should be for product reviews 2nd page url 
#
#
#####################################################################

require(stringr)||install.packages("stringr"); library(stringr)  
require(utils)||install.packages("utils"); library(utils)  


#amazon.com= function(url,    # 2nd page review url of product
 #                   n)      # Number of pages to extarct
  
#  {   
 # out <- tryCatch(
  
  #text_page=character(0)  # Create blank document
  
 # pb <- txtProgressBar(min = 1, max = n, style = 3)    # define progress bar  
  
  #url = unlist(str_split(url,"ie="))[1]   # Process url
  #url = substr(url,1,nchar(url)-1)        # Process url
  
 #for(p in 1:n){
    
    #url0 = paste(url,p,"?ie=UTF8&pageNumber=",p,"&showViewpoints=0&sortBy=byRankDescending",sep="") # Create final url
  #  ur10=paste(url,p,sep="")
    text_page=""
    url="http://www.amazon.com/GoPro-CHDHY-401-HERO4-SILVER/product-reviews/B00NIYJF6U/ref=cm_cr_pr_btm_link_2?ie=UTF8&showViewpoints=1&sortBy=recent&reviewerType=all_reviews&formatType=all_formats&filterByStar=all_stars&pageNumber=16"
    ur10=url
    text = readLines(ur10)   # read Url

    
  #  text_start = grep('<div class="reviewText">',text)  # Review start marker
  text_start = grep('<span class="a-size-base review-text">',text) 
# text_stop = grep("<div style=\"padding-top: 10px; clear: both; width: 100%;\">", text) # review end marker
text_stop = grep('</span></div><div class="a-row a-spacing-top-small review-comments">', text)    
  #  if (length(text_start) == 0) break   # check for loop termination
    
    for(j in 1:length(text_start))   # process reviews
    {
      text_temp = paste(paste(text[(text_start[j]):(text_stop[j])]),collapse=" ")
      text_page = c(text_page,text_temp)
    }
    
  #  setTxtProgressBar(pb, p)             # print progress bar
    
    #Sys.sleep(1)
  #}
  
  text_page =gsub("<.*?>", "", text_page)       # regex for Removing HTML character 
  
  text_page = gsub("^\\s+|\\s+$", "", text_page) # regex for removing leading and trailing white space
  
 # return(text_page)       # return reviews
 #)
#return(out)
#}
