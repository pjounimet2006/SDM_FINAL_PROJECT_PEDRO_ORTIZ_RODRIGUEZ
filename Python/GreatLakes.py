# importing requests, BeautifulSoup and SoupStrainer Libraries
import requests
import bs4
from bs4 import BeautifulSoup, SoupStrainer
import os
os.chdir('C:/smd/new_data')
os.getcwd()
# Creating a list to capture all the required details 
final = [['Posting_Link', 'Price', 'Year', 'Contact', 'ZipCode', 'Class', 'Category', 'Length','Make', 'Material', 'Fuel']]
# Providing the url of the home page
home_page = 'http://www.boattrader.com'
# Providing the search all page url
search_url = 'http://www.boattrader.com/search-results/NewOrUsed-any/Type-all/Category-all/Zip-60064/Radius-400/Sort-Length:DESC'
item_links = []
page_number = 1
count = 1
link_count = 0
item_year_count = 0
item_contact_count = 0
print(page_number)
# Entering the loop for pages
while page_number<357:
	try:
		r_search = requests.get(search_url)
		ad_page_html = r_search.text
		soup = BeautifulSoup(ad_page_html, 'html.parser')
#obtaining the links in each page 
		item_links = [a.attrs.get('href') for a in soup.select('a.main-link[href]')]
		count = len(item_links)
		print(count)
		page_number += 1	
		link_count += count
#obtaining the url of the next page
		next_button = [a.attrs.get('href') for a in soup.select('a.next')]
		print(next_button[0])
		next_page_link = home_page + next_button[0]
		search_url = next_page_link
		item_year = []
		item_contact = []
		item_year_text = []
		item_class = []
		item_category = []
		item_make = []
		item_length = []
		item_material = []
		item_fuel = []
		item_price = []
		item_zipcode = []
# Loop to obtain the item details for all the item urls obtained in the above loop 
		for i in range(0,count-1):
			try:
				raw_item_url = (item_links[i])
				item_url =  raw_item_url
				r_item = requests.get(item_url)
				item_page_html = r_item.text
				soup = BeautifulSoup(item_page_html, 'html.parser')
				amount = soup.find_all('span', {'class':'bd-price contact-toggle'})
				price = amount[0].get_text().strip()
				item_price.append(price)
				raw_year = soup.find_all("span",{"class":"bd-year"})
				year_text = raw_year[0].get_text().strip()
				item_year.append(year_text)
				zip_code = soup.find_all("span",{"class":"postal-code"})
				zipcode_text = zip_code[0].get_text().strip()
				item_zipcode.append(zipcode_text)
				raw_contact = soup.find_all('div',{'class':'contact'})
				contact_text = raw_contact[0].get_text().strip()
				item_contact.append(contact_text)
				table = soup.find_all('div', {'class':'collapsible open'})
				table_elem = table[0].find_all('td')
				boat_class = table_elem[0]
				boat_class_text = boat_class.get_text().strip()
				item_class.append(boat_class_text)
				boat_category = table_elem[1]
				boat_category_text = boat_category.get_text().strip()
				item_category.append(boat_category_text)
				boat_make = table_elem[4]
				boat_make_text = boat_make.get_text().strip()
				item_make.append(boat_make_text)
				boat_length = table_elem[5]
				boat_length_text = boat_length.get_text().strip()
				item_length.append(boat_length_text)
				boat_material = table_elem[6]
				boat_material_text = boat_material.get_text().strip()
				item_material.append(boat_material_text)
				boat_fuel = table_elem[7]
				boat_fuel_text = boat_fuel.get_text().strip()
				item_fuel.append(boat_fuel_text)
				all_data = [[item_url, price, year_text, contact_text, zipcode_text, boat_class_text, boat_category_text, boat_make_text, boat_length_text, boat_material_text, boat_fuel_text]]
				final += all_data
				i += 1
			except:
				i +=1
				continue
	except:
		print("entered page exception")
		page_number += 1
		continue
		
# Writing the data extracted to a CSV file
import csv
with open('Output_File_New_Great_LAkes.csv', 'w') as fp:
	b = csv.writer(fp, delimiter=',')
	b.writerows(final)
