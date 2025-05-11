# MyLangs – Language Localization Component for Delphi
MyLangs is a Delphi component designed to simplify and streamline the localization of multilingual applications.
It allows you to manage languages, assign translations, and track changes within forms and units in a structured and developer-friendly way.

🔧 Installation:
If an older version is installed, remove it from the IDE using the clean method.
Compile MyLangs_RT.dpk, then compile and install MyLangs_DT.dpk.

🛠 Development:
The project is still under development.
By tracking component additions, removals, and modifications on forms — as well as unit deletions and renaming — 
the component will become more efficient and user-friendly in future updates.

Methods:

ChangeLanguage: Translates the application into the specified language.  
MyLangs1.ChangeLanguage('Turkish');

ChangeToActiveLanguage: Changes to the currently active language.  
MyLangs1.DefaultLanguage := 'Japanese';  
MyLangs1.ChangeToDefaultLanguage;

Sample Usage:  
ComboBox1.Items.Assign(MyLangs1.Languages);  
ComboBox1.Items.Insert(0, 'DefaultValue');

MyLangs1.ChangeLanguage(ComboBox1.Items[ComboBox1.ItemIndex]);  
Or  
MyLangs1.ChangeLanguage('English');


📌 **Note:** This component was developed with the help of AI tools and public answers from Stack Overflow.  
Source references were respected where applicable.


![image](https://github.com/user-attachments/assets/6df11289-d430-42b7-93b7-a4301343bce0)
![image](https://github.com/user-attachments/assets/235d4fc0-9530-4b35-b490-00607545899a)
![image](https://github.com/user-attachments/assets/e546bacc-eae7-4000-8c88-3f8d9e6e8a64)
![image](https://github.com/user-attachments/assets/b470bab1-2e7d-486b-881e-f1d3c6d453ee)
![image](https://github.com/user-attachments/assets/1589ab5a-17fe-4247-9995-be4dd0bf8ce8)
![image](https://github.com/user-attachments/assets/ef207279-e84f-4548-86f3-9ec6cdb7996c)




