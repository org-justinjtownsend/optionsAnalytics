#' helpers cool.
#'
#' @description A set of helpers for loading the options data to notification.
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

SPX.CRON <- cronR::cron_rscript(paste0(Sys.getenv("HOME"), "/optionsAnalytics/data-raw/", "fct_getIndexData.R"))

cronR::cron_add(command = SPX.CRON,
                frequency = 'daily',
                id = 'SPX2',
                at = '01:30',
                days_of_week = c(2, 3, 4, 5, 6))

# import requests
# import os
# 
# from os.path import join, dirname
# from dotenv import load_dotenv
# 
# dotenv_path = join(dirname(__file__), '.env')
# load_dotenv(dotenv_path)
# 
# access_token = os.getenv('ACCESS_TOKEN')
# api_url_base = 'https://api.linkedin.com/v2/'
# urn = os.getenv('URN')
# author = f"urn:li:person:{urn}"
# 
# headers = {'X-Restli-Protocol-Version': '2.0.0',
#   'Content-Type': 'application/json',
#   'Authorization': f'Bearer {access_token}'}
# 
# 
# def post_on_linkedin():
#   api_url = f'{api_url_base}ugcPosts'
# 
# post_data = {
#   "author": author,
#   "lifecycleState": "PUBLISHED",
#   "specificContent": {
#     "com.linkedin.ugc.ShareContent": {
#       "shareCommentary": {
#         "text": "This is an automated share by a python script"
#       },
#       "shareMediaCategory": "NONE"
#     },
#   },
#   "visibility": {
#     "com.linkedin.ugc.MemberNetworkVisibility": "CONNECTIONS"
#   },
# }
# 
# response = requests.post(api_url, headers=headers, json=post_data)
# 
# if response.status_code == 201:
#   print("Success")
# print(response.content)
# else:
#   print(response.content)
# 
# 
# post_on_linkedin()