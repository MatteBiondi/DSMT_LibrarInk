import json
from random import randrange
import mysql.connector
import requests as r

USER='librarink'
PASSWORD='root'
DATABASE='librarink'
DATABASE_HOST='172.18.0.28'
SERVER_HOST='localhost'
ADMIN='admin@librarink.it'
ADMIN_PASSWORD='root'

URL = f"http://{SERVER_HOST}:8080/librarink-web/request/async"
JSON_FILE = 'book_info_final.json'

def connect_db():
    db = None
    try:
        db = mysql.connector.connect(
            host=DATABASE_HOST,
            user=USER,
            password=PASSWORD,
            database=DATABASE
        )
    except Exception as ex:
        print(ex)
    return db

def upload_books_mysql():

    db = connect_db()

    books = []
    with open(JSON_FILE, 'r') as json_file:
        books = json.load(json_file)

    cursor = db.cursor()

    for i,book in enumerate(books):
        values = []
        try:
            values = [book['isbn']]
            if book['totalItems'] > 0:
                item = book['items'][0]
                volume_info = item['volumeInfo']
                values.append(f"{volume_info['title']}")
                values.append((volume_info['authors'][0]))
                values.append(''.join(volume_info['categories']))
                values.append(volume_info['publishedDate'])
                values.append(volume_info['publisher'])
                values.append('English')
                values.append(volume_info['description'])
                values.append(volume_info['imageLinks']['smallThumbnail'])
                values.append(volume_info['imageLinks']['thumbnail'])
                values.append(volume_info['imageLinks']['thumbnail'])
        except Exception as ex:
            pass # print(f'Error: {ex}')

        try:
            if len(values) == 11 and len(values[1]) <= 100:
                print (f"Book #{i}")
                cursor.execute(f"INSERT INTO book (isbn, title, author, genre, year_of_publication,"
                               f"publisher, language, description,image_url_s,"
                               f"image_url_m,"
                               f"image_url_l) VALUES {tuple(values)}")
            else:
                pass # print("Skip")
        except Exception as ex:
            pass #print(ex)

    db.commit()

    print(cursor.rowcount, "record inserted")


def upload_book_copies_mnesia():
    db = connect_db()
    cursor = db.cursor()
    cursor.execute("select isbn from book")
    books = cursor.fetchall()
   
    session = r.Session()
    login = session.post(f"http://{SERVER_HOST}:8080/librarink-web/login", data={"email": ADMIN,"password":ADMIN_PASSWORD, "adminCheck":"adminLogin"}).status_code

    if login != 200:
        print("Login failed")
        return
    
    with open('responses.txt', 'w') as out:
        for index, isbn_ in enumerate(books):
            isbn = isbn_[0]
            ids = randrange(10) + 1
            for id in range(0, ids):
                try:
                    response = session.post(URL, data={'request': 'write_copy', 'isbn': isbn}).text
                    out.write(f'{isbn},{response},{ids}\n')
                except Exception as e:
                    print(e)         
            print(f'{index} -> <{isbn}, {ids}>')


if __name__ == '__main__':
    upload_books_mysql()
    upload_book_copies_mnesia()

