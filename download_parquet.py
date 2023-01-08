import requests
import re
from urllib.request import urlopen
import os

result = requests.get("https://mapmob.eic.cefet-rj.br/data/busdata/database/")

number_of_days = 100

data = str(result.content)

x = re.findall("G1\-20[0-9\-]*\.parquet", data)[::-1]

files_to_down = x[1:number_of_days]

for parquet in files_to_down:
    pp = urlopen(f"https://mapmob.eic.cefet-rj.br/data/busdata/database/{parquet}")
    with open(f'{os.getcwd()}/data/{parquet}','wb') as output:
        output.write(pp.read())
