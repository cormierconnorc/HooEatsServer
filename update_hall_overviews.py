#Update the dining hall overview in the database


import database
from dining_objs import *

def add_info(line):
	global long_line
	line_parts = line.strip().split("|")
	hall_obj = DiningHallOverview(line_parts[0], line_parts[1], line_parts[2], line_parts[3], line_parts[4], (line_parts[5] if line_parts[4] == "2" else ""))
	database.insert_overview(hall_obj)
	
in_file = open("dining_info_file.txt")

for line in in_file:
	if len(line.strip()) != 0 and line[0] != "#":
		add_info(line)
