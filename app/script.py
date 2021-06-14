from Historic_Crypto import HistoricalData
from Historic_Crypto import Cryptocurrencies
from Historic_Crypto import LiveCryptoData
from datetime import datetime, timedelta

# Get BitCoin data from the last 180 days as csv
d = datetime.today() - timedelta(days=180)
data = HistoricalData('BTC-USD',21600, d.strftime("%Y-%m-%d-00-00")).retrieve_data()
data.to_csv("data.csv")
