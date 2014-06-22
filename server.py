#The webserver. Super cool stuff.

import http.server
import socketserver
from urllib.parse import parse_qs
import scraper
import threading
import database
import time
from dining_objs import *

cache = None

class StupidHTTPRequestHandler(http.server.BaseHTTPRequestHandler):
	def build_response(self, param_dict):
		global cache
		#Just get dummy data
		if param_dict.get("halls") != None and "all" in param_dict["halls"]:
			#return open("scraper.out").read()
			return get_json(cache)
		else:
			return ""

	def do_GET(self):
		#Get parameters from request:
		params = self.path.split("?")
		params = parse_qs(params[1] if len(params) > 1 else "")
		print(params)

		response = self.build_response(params)

		#Respond
		self.send_response(200)
		self.send_header("Content-type", "text/plain")
		self.end_headers()
		self.wfile.write(bytes(response, "UTF-8"))


def refresh_cache():
	global cache
	while True:
		cache = database.get_dining_halls()
		print("Finished getting cache at: " + time.strftime("%H:%M:%S"))
		time.sleep(60 * 37)

#Start the scraper as a thread. DON'T RUN THIS OVER THE SUMMER!
#scraper_thread = threading.Thread(target=scraper.run_scraper)
#scraper_thread.daemon = True
#scraper_thread.start()

#Start cache refresher as a thread:
refresh_thread = threading.Thread(target=refresh_cache)
refresh_thread.daemon = True
refresh_thread.start()
		
httpd = socketserver.TCPServer(("", 8000), StupidHTTPRequestHandler)
print("Starting at port 8000")
httpd.serve_forever()
