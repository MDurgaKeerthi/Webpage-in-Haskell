# Webpage-in-Haskell

# Webpage in Haskell using Yesod framework

Steps to run app in localhost mode <br/>
$stack build <br/>
$stack exec -- yesod devel

App folder:
The app folder contains the files:
1. devel.hs - This calls the develMain function. The develMain function is present in Application.hs. It calls the getApplicationDev function that returns the Warp settings and WAI Application.
2. DevelMain.hs - This contains the start and shutdown of the app. It creates threads for the start of each new server.
3. main.hs - This calls the appMain function. The appMain function is present in Application.hs. It loads all the settings and runs the app.

Config Folder:
The config folder contains the files:
1. routes - This contains the path of all the handler functions. Whenever a handler function is called the path is obtained from the routes file.
2. models - This contains the list of all the entities in the database.

Handler Folder:
The handler folder contains the files:
1. AllowTrans.hs - This is called after successful login. This contains the links for transaction,paybill and also contains information about the last 3 transactions and the balance of the user.
2. CheckPassword.hs - This is called to check the input password with the password in the database.
3. Opinion.hs - This is called for inserting the feedback into the database.
4. Contact.hs - This directs to the contact.hamlet file that contains the contact information.
5. Details.hs - This is called for modifing the user details. This contains a form for modifing the pasword, name and age of the user.
6. Feedback.hs - This creates a feedback form whose entries are stored in the database.
7. Home.hs - This directs to the homepage.hamlet file that displays the home page.
8. Instructions.hs - This directs to the instructions.hamlet file that contains instructions.
9. Paybill.hs - This is called for paying bills. It contains a form where we need to enter the type of bill, the region number and the amount to be transfered. It then updates the database entries based on the input and directs to suc_bill.hs on successful transfer.
10. Profile.hs - This contains the form for entering the password. For a new user it calls setpassword and for an old user it calls checkpassword. 
11. Suc_bill.hs - This is called after a successful billing transaction.This directs to the billpaid.hamlet file. The hamlet file has links for transaction,paying bill or for viewing details.
12. Suc_change.hs - This is called after successful change of details. It directs to the suc_change.hamlet file.
13. Suc_trans.hs - This is called after a successful transaction between user accounts. It directs to the transfer.hamlet file.
14. Transaction.hs - This is called for transferring money between user accounts. It contains a form where we need to enter the bankaccount number of the receiver and the amount to be transferred.It updates the database entries based on the input and directs to suc_trans.hs after a successful transfer.
15. TryAgain.hs - This is called when the input password is incorrect and it directs to tryagain.hamlet file.
 
Import Folder:
The import folder contains the Nofoundation.hs file that imports all the packages that are not present in foundation.hs.

Settings Folder:
The settings folder contains the StaticFiles.hs file that generates easy references to files in the static directory at compile time giving you compile-time verification that referenced files exist. 

Static Folder:
This folder contains the css files, fonts and the images that we need for our website.

templates Folder:
This folder contains the shakespearen templates i.e the hamlet,lucius and juliuis files that we need for our website.

Application.hs:
This file  contains declarations of the functions used by appMain,develmain and DevelMain.hs that are required for initializing and running the app inside ghci. It also allocates resources such as data base connection pool and creates a database connection pool. It then converts the foundation into WAI application.

Foundation.hs:
This file contains the declarations for our main foundation datatype for our application. It also contains declarations to the items on the menu bar. We import all the routes in this file and we specify the the authentication for the routes. This contains the runDB function for running the database. It also contains the initialization of the user entities in the database. It also specifies the paths on login and on logout.

Import.hs:
This file contains the imports to foundation.hs and nofoundation.hs so that they can be used in all handler functions.

Model.hs:
This file contains the import of the database entities defined in the models file.

project1.cabal file:
This contains the  names of all the files and versions of all the dependencies of the executable file.

Settings.hs:
This file contains all the runtime settings to configure this application.It contains the settings for widgetfile,configsettings and settings for combining multiple css files during compile time. 
