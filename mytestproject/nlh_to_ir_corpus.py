from bs4 import BeautifulSoup
import os


def write_sub_headers_with_text_to_individual_file():
    headers3 = soup.find_all('h3')
    for head3 in headers3:
        try:
            value = head3.string.replace("/","-")
            value = value.replace(" ", "")
            value = value.replace("*", "")
            file = open("output/" + value + ".txt", "wb")
            file.write(bytes(head3.parent.get_text(), 'utf-8'))
            file.flush()
            file.close()
        except FileNotFoundError:
            print("invalid filename: " + head3.string + ". File not created.")
        except TypeError:
            print("got a NoneType.")
        except AttributeError:
            print("No method .replace on NoneType.")

path = '/Users/andreas/Documents/Sommerjobb_2014/Datakilder/NLH/NLH-html-20130925-01/'

#soup.find_all('h1') # Norsk legemiddelhandbok
#soup.find_all('h2') # LA.B, ex L2.1
#soup.find_all('h3') # LA.B.C, ex L2.1.1
#soup.find_all('h4') # LA.B.C.D, ex L2.1.1.1
#soup.find_all('h5') # andre overskrifter, ikke indeksert.
#soup.find_all('h6') # preparater

for filename in os.listdir(path + 'T/'):
     soup = BeautifulSoup(open(path + 'T/' + filename, encoding='iso-8859-1'), 'html', from_encoding='iso-8859-1')
     write_sub_headers_with_text_to_individual_file()

print("finished writing T.")

for filename in os.listdir(path +  'L/'):
     soup = BeautifulSoup(open(path + 'L/' + filename, encoding='iso-8859-1'), 'html', from_encoding='iso-8859-1')
     write_sub_headers_with_text_to_individual_file()

print("finished writing L.")

for filename in os.listdir(path + 'G/'):
     soup = BeautifulSoup(open(path + 'G/' + filename, encoding='iso-8859-1'), 'html', from_encoding='iso-8859-1')
     write_sub_headers_with_text_to_individual_file()

print("finished writing G.")