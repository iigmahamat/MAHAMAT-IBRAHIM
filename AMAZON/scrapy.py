# -*- coding: utf-8 -*-
"""
Created on Tue Nov  2 00:04:33 2021

@author: hp
"""
import os
import requests
from bs4 import BeautifulSoup
from untitled5 import amazonbot
from pymongo import MongoClient 
from dotenv import load_dotenv # creer des variables d'environement 

load_dotenv()
# code pour se connecter à notre MONGODB clusters
# une fonction de connexion à notre mongodb
try:
    client = MongoClient("mongodb+srv://"+os.getenv("MONGODB_USERNAME")+\
        ":"+os.getenv("MONGODB_PASSWORD")+"@"+os.getenv("MONGODB_DOMAIN")+\
            "/"+os.getenv("MONGODB_DBNAME")+"?retryWrites=true&w=majority")
    client.server_info()
except Exception as e:
    raise e # raise permet de declencher l'exception dans le cas où on a pas pu se connecter a mongodb
import sys    # pour que le code s'arrete juste après la connexion a mongodb cluster
sys.exit(0)
    
bot = amazonbot(mongodb_client=client)
#product_urls={
 #   "https://www.amazon.fr/dp/B094WLFGD3/ref=redir_mobile_desktop?_encoding=UTF8&ref_=cm_cr_othr_d_bdcrb_top#cm_cr_carousel_images_section"
  #  "https://www.amazon.fr/PlayStation-Digital-Manette-DualSense-Couleur/dp/B08H98GVK8/ref=sr_1_1?brr=1&pd_rd_r=313502e0-195f-48ff-bdae-443052ad4b30&pd_rd_w=8hc6w&pd_rd_wg=KVU7J&qid=1636038015&rd=1&s=videogames&sr=1-1",
   # "https://www.amazon.fr/Electronic-Arts-5030946123858-FIFA-PlayStation/dp/B0992JHMSX/ref=sr_1_2?brr=1&pd_rd_r=21125276-436d-4124-bf30-9795d61c5658&pd_rd_w=TYP1X&pd_rd_wg=F4nhw&qid=1636036146&rd=1&s=videogames&sr=1-2",
    #"https://www.amazon.fr/Call-Duty-Vanguard-PlayStation-Exclusivit%C3%A9/dp/B09D8MBVQ5/ref=pd_sbs_8/259-0829623-6354535?pd_rd_w=bmfmF&pf_rd_p=81cea6b9-d758-4c09-83c8-cd14bfe8b85f&pf_rd_r=GWQBBAJ0A7A5EYATGJV8&pd_rd_r=027bbcc8-3d7a-4e67-a1bb-44eb293f7196&pd_rd_wg=1vBwI&pd_rd_i=B09D8MBVQ5&psc=1",
   # "https://www.amazon.fr/KUTUKU-RRS-00007-Xbox-Series-S/dp/B08GD9MNZB/ref=sr_1_3?brr=1&pd_rd_r=ceea0e59-1008-48a9-bc45-b0646e1a779b&pd_rd_w=UvZiv&pd_rd_wg=yeKd2&qid=1636036204&rd=1&s=videogames&sr=1-3",
   # "https://www.amazon.fr/NBA-2K22-Exclusivit%C3%A9-Amazon-PlayStation/dp/B099KWPDVS/ref=pd_vtp_2/259-0829623-6354535?pd_rd_w=X9jam&pf_rd_p=ae9723b3-1b9c-43b5-84ad-70af60d95378&pf_rd_r=GWQBBAJ0A7A5EYATGJV8&pd_rd_r=027bbcc8-3d7a-4e67-a1bb-44eb293f7196&pd_rd_wg=1vBwI&pd_rd_i=B099KWPDVS&psc=1",
   # "https://www.amazon.fr/Console-Nintendo-dAccueil-Manettes-Blanches/dp/B098RJXBTY/ref=sr_1_1?_encoding=UTF8&c=ts&keywords=Consoles+pour+Nintendo+Switch&qid=1636036247&s=videogames&sr=1-1&ts_id=12366241031"
  #  }
bot.scrap_urls()

 