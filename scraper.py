#Dining Menu Web Scraper

from bs4 import BeautifulSoup
from urllib.request import urlopen
import sys
from dining_objs import *
import json
from database import *
import time


def get_first_child(tag):
	return next(tag.children)
	
def move_down(tag, level):
	for x in range(level):
		if tag == None:
			return None
		tag = get_first_child(tag)
	return tag

def get_page(url):
	req = urlopen(url)
	return req.read()

def get_content_list(table):
	#Find the station names, stored in an <a> tag at the table's top level
	items = table.find_all('a', recursive=False)
	item_list = []
	for item in items:
		item_str = str(item.string).strip()
		item_str = item_str[:len(item_str) - (2 if item_str[len(item_str) - 1] == ">" else 0)].strip()
		item_list.append(item_str)
	return item_list

def get_station_nutrition(table):
	nut_sections = table.find_all('div', {"class" : "section"}, recursive=False)
	for x in range(len(nut_sections)):
		nut_sections[x] = move_down(nut_sections[x], 2)
	#Now get the nutritional information for each item:
	nut_list = []
	for item_nuts in nut_sections:
		my_nuts = []
		#The actual nutritional information. Pull info from "item" attrs
		for nut_tag in item_nuts.children:
			if nut_tag.has_attr('n') and nut_tag.has_attr('v'):
				name = str(nut_tag['n']).strip()
				value = str(nut_tag['v']).strip().replace('Â\xa0', ' ')
				#Make sure relevant attrs have valid values
				if name != "" and value != "" and value.find("99999.99") == -1:
					my_nuts.append(name + ": " + value)
		nut_list.append(my_nuts)
	return nut_list
	
def get_station_menus(table):
	#Tables containing menu items at each station, indices correspond with station
	station_tables = table.find_all('div', recursive=False)
	station_menus = []
	for table in station_tables:
		items = get_content_list(table)
		nutritional_info = get_station_nutrition(table)
		#Zip into a single list
		items = list(zip(items, nutritional_info))
		station_menus.append(items)
	return station_menus

def build_menu(table):
	"""Menu format: [[station, [item, [nutrition]]]]"""
	stations = get_content_list(table)
	station_menus = get_station_menus(table)
	return list(zip(stations, station_menus))
	
def get_menu(name, id):
	url = "http://www.campusdish.com/en-US/CSMA/Virginia/Home.htm?LocationID=" + id
	html = get_page(url)
	
	soup = BeautifulSoup(html)

	has_dinner = len(soup.find_all("img", id="DinnerICN")) != 0
	has_brunch = len(soup.find_all("img", id="BrunchICN")) != 0

	breakfast, brunch, lunch, dinner = None, None, None, None
	
	first = move_down(soup.find("table", id="menu1"), 2)
	second = move_down(soup.find("table", id="menu2"), 2)
	third = move_down(soup.find("table", id="menu3"), 2)

	#These could be swapped based on which is present
	if has_dinner and not has_brunch:
		lunch = first
		dinner = second
		breakfast = third
	elif has_brunch and not has_dinner:
		brunch = first
	elif has_brunch:
		dinner = first
		brunch = second
	else:
		lunch = first
		breakfast = second

	full_menu = []

	if lunch != None:
		full_menu.append(["Lunch", build_menu(lunch)])
	if dinner != None:
		full_menu.append(["Dinner", build_menu(dinner)])
	if breakfast != None:
		full_menu.append(["Breakfast", build_menu(breakfast)])
	if brunch != None:
		full_menu.append(["Brunch", build_menu(brunch)])

	return [name, full_menu]

def pull_options(home_page):
	soup = BeautifulSoup(get_page(home_page))
	select = soup.find("select", {"name" : "WucChalkboard1:findLocations"})
	children = select.find_all("option")
	options_list = []
	for bitch_please in children:
		id = bitch_please['value']
		hall_name = str(bitch_please.string)
		options_list.append([hall_name, id])
	return options_list

def convert_list(menu_list):
	dining_halls = []
	for hall in menu_list:
		dining_halls.append(DiningHall(hall[0], [Meal(meal[0], [Station(station[0], [Item(item[0], item[1]) for item in station[1]]) for station in meal[1]]) for meal in hall[1]]))
	return dining_halls
		
	

def do_scrape():
	options = pull_options("http://www.campusdish.com/en-us/CSMA/VIRGINIA")
	menu_list = []
	for option in options:
		menu_list.append(get_menu(option[0], option[1]))
	return convert_list(menu_list)

def run_scraper():
	while True:
		dining_halls = do_scrape()
		#json = get_json(dining_halls)
		#open("scraper.out", "w").write(json)
		insert_all(dining_halls)
		print("Scraper run complete at " + str(time.strftime(("%H:%M:%S"))))
		#Sleep between tests
		time.sleep(60 * 60)
	
