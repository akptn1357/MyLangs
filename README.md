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
