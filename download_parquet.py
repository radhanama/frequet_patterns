import requests
import re
from urllib.request import urlopen
from dotenv import load_dotenv
import os
import json

load_dotenv()

result = requests.get("https://mapmob.eic.cefet-rj.br/data/busdata/database/")

data = str(result.content)

x = re.findall("G1\-20[0-9\-]*\.parquet", data)[::-1]

files_to_down = x[1:int(os.environ["FilesAmount"])]

with open(f'{os.environ["FilesPath"]}/files_to_down.json','w') as output:
    output.write(json.dumps(files_to_down))

for parquet in files_to_down:
    pp = urlopen(f"https://mapmob.eic.cefet-rj.br/data/busdata/database/{parquet}")
    with open(f'{os.environ["pathFiles"]}/data/{parquet}','wb') as output:
        output.write(pp.read())
