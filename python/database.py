import pymysql
from dining_objs import *
#functions:
#set_active(*arg) parameters are names of foods to make active
#set_unactive(*arg) paramters are names of foods to make unactive
#add_food(full_name, categories_id, nutritional_info)

#Open database connection													 
db = pymysql.connect("localhost", "serveman", "uvahacks", "HooEats")

# prepare a cursor object using cursor() method								
cursor = db.cursor()


def commit(sql):
	global db
	global cursor
	try:
		# Execute the SQL command										 
		cursor.execute(sql)
		#Commit your changes in the database							   
		db.commit()
		#Note successful query
		return True
	except:
		# Rollback in case there is any error							 
		db.rollback()
		print("Query failed: " + sql)
		#Note failed query
		return False

#make a food active
def set_active(*arg):
	#update which foods are active
	sql = """UPDATE Items SET active=1 WHERE """
	for i in range(len(arg)):
		if(i==0):
			sql=sql+"name='"+arg[i]+"'"
		else:
			sql=sql+"OR name='"+arg[i]+"'"
	
	print(sql)
	commit(sql)
						 

def set_unactive(*arg):
	#update which foods are unactive
	sql = """UPDATE Items SET active=0 WHERE """
	for i in range(len(arg)):
		if(i==0):
			sql=sql+"name='"+arg[i]+"'"
		else:
			sql=sql+"OR name='"+arg[i]+"'"
	print(sql)
	commit(sql)

def clear_tables():
	commit("DELETE FROM Items")
	commit("DELETE FROM Stalls")
	commit("DELETE FROM Meals")
	commit("DELETE FROM DiningHalls")

def make_all_unactive():
	#update which foods are active
	sql = """UPDATE Items SET active=0"""
	commit(sql)
	sql = "UPDATE Stalls SET active=0"
	commit(sql)
	sql = "UPDATE Meals SET active=0"
	commit(sql)
	sql = "Update DiningHalls SET active=0"

def add_food(name, stall, nutrition):
	#update which foods are active
	sql = "INSERT INTO Items (name, stall,nutrition) VALUES ('{0}',{1},'{2}')".format(name, stall, nutrition)
	print(sql)
	commit(sql)		
		
def get_all_active_foods():
	sql = "SELECT * FROM Items WHERE active=1"
	commit(sql)
	return cursor.fetchall()

def get_all_active_dining_halls():
	sql = "SELECT * FROM DiningHalls WHERE active=1"
	commit(sql)
	return cursor.fetchall()
	
def get_all_active_meals():
	sql = "SELECT * FROM Meals WHERE active=1"
	commit(sql)
	return cursor.fetchall()
	
def get_all_active_stalls():
	sql = "SELECT * FROM Stalls WHERE active=1"
	commit(sql)
	return cursor.fetchall()

def get_all_active_items():
	sql = """SELECT DISTINCT * FROM Items, DiningHalls, Meals, Stalls 
	WHERE Items.active=1 AND DiningHalls.active=1 AND Meals.active=1 AND Stalls.active=1"""
	commit(sql)
	return cursor.fetchall()


def get_dining_halls():
	foods = get_all_active_foods()
	stalls = get_all_active_stalls()
	meals = get_all_active_meals()
	dining_halls = get_all_active_dining_halls()
	
	oitems = []
	ostalls=[]
	omeals = []
	odininghalls = []
	
	#items are complete
	for f in range(len(foods)):
		tempi = Item(foods[f][1], [item.strip() for item in foods[f][4][1:len(foods[f][4]) - 1].split(',')])
		oitems.append(tempi)
	
	#dining halls missing meals
	for d in range(len(dining_halls)):
		tempd = DiningHall(dining_halls[d][1], list())
		odininghalls.append(tempd)

	#meals missing stalls
	for m in range(len(meals)):
		tempm = Meal(meals[m][1], list())
		omeals.append(tempm)

	#stalls missing items
	for s in range(len(stalls)):
		temps = Station(stalls[s][1], list())
		ostalls.append(temps)


	#match food's stalls with stall
	for f in range(len(foods)):
		for s in range(len(stalls)):
			if foods[f][2] == stalls[s][0]:
				ostalls[s].items.append(oitems[f])
				
	for s in range(len(stalls)):
		for m in range(len(meals)):
			if stalls[s][2] == meals[m][0]:
				omeals[m].stations.append(ostalls[s])

	for m in range(len(meals)):
		for d in range(len(dining_halls)):
			if meals[m][2] == dining_halls[d][0]:
				odininghalls[d].meals.append(omeals[m])
	
	return odininghalls

def get_all_dining_hall_overviews():
	sql = "SELECT * FROM DiningHallOverviews"
	overviews = []
	if commit(sql):
		d_list = cursor.fetchall()
		for row in d_list:
			overviews.append(DiningHallOverview(row[1], row[2], row[3], row[4], row[5], row[6]))
	return overviews
	
def get_last_insert_id():
	return db.insert_id()
	
def insert_dining_hall(dining_hall):
	sql = "INSERT INTO DiningHalls(name, active) VALUES (\"{:s}\", 1)".format(dining_hall.name)
	#print(sql)
	return commit(sql)

def insert_meal(dining_hall_id, meal):
	sql = "INSERT INTO Meals(name, dining_hall, active) VALUES (\"{:s}\", {:d}, 1)".format(meal.name, dining_hall_id)
	#print(sql)
	return commit(sql)

def insert_station(meal_id, station):
	sql = "INSERT INTO Stalls(name, meal, active) VALUES (\"{:s}\", {:d}, 1)".format(station.name, meal_id)
	#print(sql)
	return commit(sql)

def insert_item(station_id, item):
	sql = "INSERT INTO Items(name, stall, active, nutrition) VALUES (\"{:s}\", {:d}, 1, \"{:s}\")".format(item.name, station_id, str(item.nutrition).replace("'", ""))
	#print(sql)
	return commit(sql)
	  
def insert_all(dining_halls):
	clear_tables()
	#it may be a monstrosity, but it does account for possible errors
	for dining_hall in dining_halls:
		success = insert_dining_hall(dining_hall)
		if success:
			dining_hall_id = get_last_insert_id()
			for meal in dining_hall.meals:
				success = insert_meal(dining_hall_id, meal)
				if success:
					meal_id = get_last_insert_id()
					for station in meal.stations:
						success = insert_station(meal_id, station)
						if success:
							station_id = get_last_insert_id()
							for item in station.items:
								insert_item(station_id, item)
def overview_exists(view):
	sql = "SELECT id FROM DiningHallOverviews WHERE name=\"{:s}\"".format(view.name)
	#print(sql)
	if commit(sql):
		return len(cursor.fetchall()) >= 1
	return False
								
def insert_overview(view):
	#Update if it already exists
	if overview_exists(view):
		print("Updating existing dining hall with name {:s}".format(view.name))
		sql = "UPDATE DiningHallOverviews SET name=\"{:s}\",description=\"{:s}\",location=\"{:s}\",hours=\"{:s}\",accepts_mealswipes=\"{:s}\",mealswipe_hours=\"{:s}\" WHERE name=\"{:s}\"".format(view.name, view.description, view.location, view.hours, view.accepts_mealswipes, view.mealswipe_hours, view.name)
	#Create new if it doesn't
	else:
		print("Inserting new dining hall with name {:s}".format(view.name))
		sql = "INSERT INTO DiningHallOverviews(name, description, location, hours, accepts_mealswipes, mealswipe_hours) VALUES (\"{:s}\", \"{:s}\", \"{:s}\", \"{:s}\", \"{:s}\", \"{:s}\")".format(view.name, view.description, view.location, view.hours, view.accepts_mealswipes, view.mealswipe_hours)
	return commit(sql)

#close db
#db.close()
