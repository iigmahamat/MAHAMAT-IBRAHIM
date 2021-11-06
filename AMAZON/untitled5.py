# -*- coding: utf-8 -*-
"""
Created on Wed Nov  3 14:19:22 2021

@author: hp
"""
import time
import datetime
import requests
from bs4 import BeautifulSoup
import pprint

class amazonbot : # création d'une classe contenant nos différentes fonction 
    
    def __init__(self, mongodb_client):   # definition d'un objet 
        self.amazon_header = headers = {
    'Accept-Encoding': 'gzip, deflate, sdch',
    'Accept-Language': 'en-US,en;q=0.8',
    'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_10_1) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/39.0.2171.95 Safari/537.36',
    'Accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8',
    'Referer': 'http://www.wikipedia.org/',
    'Connection': 'keep-alive',}          # conversion de URL en requete pour detourner le blockage d'Amazon
        self.mongodb_client = mongodb_client
    def get_product_title(self,soup):    # fonction pour extraire le titre du produit  
        try:
            return soup.find('span',{'id':'productTitle'}).get_text().strip()
        except :
            return None
        
    def get_product_rating(self,soup):   # fonction pour recuperer la note sur le produit 
        try :
        
            div_avg_cust_reviews = soup.find('div', {'id': 'averageCustomerReviews'})
            rating = div_avg_cust_reviews.find('span', {'class': 'a-icon-alt'}).get_text().strip().split()[0]
            return float(rating.replace(',', '.'))
        except :
            return None
        
    def get_product_nb_reveiws(self, soup): # fonction pour extraire le nombre d'évaluateur
        try : # ajout du bloque try except pour que le code continue de fonctionner si l'elément ne figure pas dans la page a scraper 
              # le bloque try c'est un mecanisme qui permet de surveiller l'apparition d'une exception 
             nb_reviewers = soup.find('span', {'id': 'acrCustomerReviewText'}).get_text().strip()
             return int(''.join(nb_reviewers.split()[:-1]))
        except:
            return None
        
    def get_product_price(self,soup):      # une fonction pour extraire le prix du produit
        try : 
             price = soup.find('span', {'id': 'priceblock_ourprice'}).get_text().strip()
             return float(price.split()[0].replace(',', '.'))
        except:
            return None
    
    def get_product_data (self, product_url): # une fonction qui retourne une dictionnaire contenant nos données
        r = requests.get(product_url,headers=self.amazon_header)
        soup =BeautifulSoup(r.content,'html.parser')
        title = self.get_product_title(soup)
        rating = self.get_product_rating(soup)
        nb_reviewers= self.get_product_nb_reveiws(soup)
        price = self.get_product_price(soup)
        return {
        "url":product_url,
        "title" :  title, 
        "rating": rating,
        "nb_reviewers": nb_reviewers,
        "price": price,
        "update_date": datetime.datetime.now()
            }
    
    def scrap_urls(self): # une fonction qui nous permet de scrapper plusieurs produits
        product_urls = self.mongodb_client["amazon"]["product_urls"].find()
        for product_url in product_urls:
            print(product_url)
            print()
            data= self.get_product_data(product_url["url"])     # ici product_url devient un objet  
            self.mongodb_client["amazon"]["product_data"].update({"url": product_url['url']}, {"$set": data}, upsert=True)
        
    
       
    