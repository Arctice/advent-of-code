import csv

data = list(csv.DictReader(open('5784/noahs-customers.csv')))

phone_keys = str.maketrans('abcdefghijklmnopqrstuvwxyz',
                           '22233344455566677778889999')

for customer in data:
    phone = customer['phone'].replace('-', '')
    names = customer['name'].split()
    last_name = names[1]
    phone_name = last_name.lower().translate(phone_keys)
    if phone.endswith(phone_name):
        print(last_name, phone_name, customer['phone'])
        
    
