.male.names <- c(
  "Jacob", "Mason", "Ethan", "Noah", "William", "Liam", "Jayden", "Michael", "Alexander", "Aiden", "Daniel", "Matthew", "Elijah", "James", "Anthony", "Benjamin", "Joshua", "Andrew", "David", "Joseph", "Logan", "Jackson", "Christopher", "Gabriel", "Samuel", "Ryan", "Lucas", "John", "Nathan", "Isaac", "Dylan", "Caleb", "Christian", "Landon", "Jonathan", "Carter", "Luke", "Owen", "Brayden", "Gavin", "Wyatt", "Isaiah", "Henry", "Eli", "Hunter", "Jack", "Evan", "Jordan", "Nicholas", "Tyler", "Aaron", "Jeremiah", 
  "Julian", "Cameron", "Levi", "Brandon", "Angel", "Austin", "Connor", "Adrian", "Robert", "Charles", "Thomas", "Sebastian", "Colton", "Jaxon", "Kevin", "Zachary", "Ayden", "Dominic", "Blake", "Jose", "Oliver", "Justin", "Bentley", "Jason", "Chase", "Ian", "Josiah", "Parker", "Xavier", "Adam", "Cooper", "Nathaniel", "Grayson", "Jace", "Carson", "Nolan", "Tristan", "Luis", "Brody", "Juan", "Hudson", "Bryson", "Carlos", "Easton", "Damian", "Alex", "Kayden", "Ryder", "Jesus", "Cole", "Micah", "Vincent", "Max", "Jaxson", "Eric", "Asher", "Hayden", "Diego", "Miles", "Steven", "Ivan", "Elias", "Aidan", "Maxwell", "Bryce", "Antonio", "Giovanni", "Timothy", "Bryan", "Santiago", "Colin", "Richard", "Braxton", "Kaleb", "Kyle", "Kaden", "Preston", "Miguel", "Jonah", "Lincoln", "Riley", "Leo", "Victor", "Brady", "Jeremy", "Mateo", "Brian", "Jaden", "Ashton", "Patrick", "Declan", "Sean", "Joel", "Gael", "Sawyer", "Alejandro", "Marcus", "Leonardo", "Jesse", "Caden", "Jake", "Kaiden", "Wesley", "Camden", "Edward", "Brantley", "Roman", "Axel", "Silas", "Jude", "Grant", "Cayden", "Emmanuel", "George", "Maddox", "Malachi", "Bradley", "Alan", "Weston", "Gage", "Devin", "Greyson", "Kenneth", "Mark", "Oscar", "Tanner", "Rylan", "Nicolas", "Harrison", "Derek", "Peyton", "Ezra", "Tucker", "Emmett", "Avery", "Cody", "Calvin", "Andres", "Jorge", "Abel", "Paul", "Abraham", "Kai", "Collin", "Theodore", "Ezekiel", "Omar", "Jayce", "Conner", "Bennett", "Trevor", "Eduardo", "Peter", "Maximus", "Jaiden", "Jameson", "Seth", "Kingston", "Javier", "Travis", "Garrett", "Everett", "Graham", "Xander", "Cristian", "Damien", "Ryker", "Griffin", "Corbin", "Myles", "Luca", "Zane", "Francisco", "Ricardo", "Alexis", "Stephen", "Zayden", "Iker", "Drake", "Lukas", "Charlie", "Spencer", "Zion", "Erick", "Josue", "Jeffrey", "Trenton", "Chance", "Paxton", "Elliot", "Fernando", "Keegan", "Landen", "Manuel", "Amir", "Shane", "Raymond", "Zander", "Andre", "Israel", "Mario", "Cesar", "Simon", "King", "Jaylen", "Johnathan", "Troy", "Dean", "Clayton", "Dominick", "Tyson", "Jasper", "Martin", "Kyler", "Hector", 
  "Edgar", "Marco", "Cash", "Edwin", "Shawn", "Judah", "Andy", "Donovan", "Kameron", "Elliott", "Dante", "Braylon", "Anderson", "Johnny", "Drew", "Sergio", "Cruz", "Dalton", "Rafael", "Gregory", "Lane", "Erik", "Skyler", "Finn", "Reid", "Gunner", "Jared", "Caiden", "Holden", "Emilio", "Fabian", "Aden", "Brendan", "Rowan", "Emiliano", "Braden", "Jase", "Jax", "Emanuel", "Lorenzo", "Roberto", "Amari", "Angelo", "Beau", "Louis", "Derrick", "Beckett", "Dawson", "Felix", "Pedro", "Brennan", "Frank", "Maximiliano", 
  "Quinn", "Dallas", "Romeo", "Braylen", "Joaquin", "Waylon", "Allen", "Colt", "Ruben", "Milo", "Julius", "Grady", "August", "Dakota", "Cohen", "Brock", "Kellen", "Brycen", "Desmond", "Malik", "Colby", "Nehemiah", "Leland", "Jett", "Marcos", "Taylor", "Karter", "Marshall", "Ty", "Phillip", "Corey", "Ali", "Adan", "Dillon", "Arthur", "Maverick", "Leon", "Brooks", "Tristen", "Titus", "Keith", "Dexter", "Karson", "Emerson", "Landyn", "Armando", "Pablo", "Knox", "Enrique", "Cade", "Gerardo", "Reed", "Kellan", 
  "Jayson", "Barrett", "Walter", "Dustin", "Kolton", "Ronald", "Trent", "Phoenix", "Ismael", "Julio", "Danny", "Kason", "Scott", "Messiah", "Jay", "Esteban", "Gideon", "Tate", "Abram", "Trey", "Keaton", "Jakob", "Jaime", "Devon", "Braydon", "Izaiah", "Donald", "Albert", "Raul", "Darius", "Archer", "Colten", "Damon", "River", "Gustavo", "Philip", "Atticus", "Walker", "Matteo", "Randy", "Saul", "Rocco", "Davis", "Enzo", "Noel", "Orion", "Jamari", "Remington", "Bruce", "Darren", "Larry", "Mathew", "Russell", "Dennis", "Tony", "Chris", "Porter", "Rodrigo", "Armani", "Zaiden", "Kade", "Ari", "Hugo", "Zachariah", "Kamden", "Mohamed", "Quentin", "Solomon", "Curtis", "Leonel", "Issac", "Khalil", "Alberto", "Jerry", "Alec", "Gianni", "Moises", "Gunnar", "Adriel", "Lawrence", "Alijah", "Chandler", "Ronan", "Prince", "Payton", "Arturo", "Jimmy", "Orlando", "Ricky", "Mitchell", "Maximilian", "Cason", "Malcolm", "Muhammad", "Kasen", "Marvin", "Jalen", "Cyrus", "Mauricio", "Warren", "Jonas", "Kendrick", "Rhys", "Dane", "Ryland", "Pierce", "Johan", "Rory", "Uriel", "Major", "Bryant", "Reece", "Casey", "Ibrahim", "Nikolas", "Arjun", "Sullivan", "Finnegan", "Alfredo", "Royce", "Ahmed", "Amare", "Lance", "Ramon", "Jamison", "Brayan", "Brenden", "Dominik", "Case", "Kristopher", "Maurice", "Mekhi", "Kobe", "Zackary", "Rhett", "Jensen", "Jaxton", "Deandre", "Isaias", "Channing", "Yahir", "Ezequiel", "Tobias", "Talon", "Sam", "Justice", "Kash", "Nash", "Alvin", "Jacoby", "Ace", "Nico", "Quinton", "Cannon", "Franklin", "Raiden", "Joe", "Lawson", "Beckham", "Gary", "Aldo", "Raylan", "Frederick", "London", "Boston", "Carl", "Byron", "Ernesto", "Moshe", "Terry", "Eddie", "Kane", "Moses", "Finley", "Salvador", "Reese", "Kelvin", "Cullen", "Madden", "Wade", "Clark", "Mohammed", "Kieran", "Jagger", "Dorian", "Korbin", "Nelson", "Roy", "Asa", "Matias", "Nasir", "Nickolas", "Roger", "Alonzo", "Jaxen", "Skylar", "Callen", "Malakai", "Douglas", "Ahmad", "Uriah", "Conor", "Kristian", "Carmelo", "Blaine", "Kayson", "Bentlee", "Braeden", "Julien", "Nathanael", "Aarav", "Keagan", "Lucian", "Morgan", "Chad", "Terrance", "Benson", "Noe", "Rodney", "Francis", "Layne", "Mohammad", "Zayne", "Tatum", "Brett", "Wilson", "Kian", "Marc", "Rohan", "Dayton", "Braiden", "Harper", "Luciano", "Nikolai", "Kamari", "Camron", "Joey", "Santino", "Ellis", "Layton", "Xzavier", "Jefferson", "Winston", "Guillermo", "Demetrius", "Bowen", "Daxton", "Melvin", "Soren", "Neil", "Sylas", "Jon", "Raphael", "Rex", "Yusuf", "Shaun", "Brodie", "Tommy", "Harley", "Quincy", "Dax", "Trace", "Adonis", "Bently", "Giovani", "Jeffery", 
  "Odin", "Luka", "Kylan", "Willie", "Lewis", "Tripp", "Vihaan", "Davion", "Kendall", "Arian", "Cory", "Jamarion", "Jonathon", "Nixon", "Rayan", "Emery", "Jermaine", "Reginald", "Tomas", "Emmitt", "Ayaan", "Zechariah", "Billy", "Hamza", "Micheal", "Urijah",
  "Aryan", "Lee", "Jasiah", "Landry", "Crosby", "Mathias", "Toby", "Tristian", "Will", "Felipe", "Triston", "Eden", "Terrell", "Deacon", "Matthias", "Jamal", "Makai", "Maxim", "Sterling", "Hank", "Gerald", "Alessandro", "Jaydon", "Hayes", "Niko", "Branson", 
  "Flynn", "Kody", "Marlon", "Mayson", "Allan", "Augustus", "Jessie", "Neymar", "Adrien", "Aydan", "Leonard", "Sincere", "Kyson", "Terrence", "Jerome", "Jadiel", "Kole", "Aron", "Aydin", "Omari", "Ronnie", "Zain", "Vicente", "Bobby", "Yosef", "Alexzander", 
  "Harry", "Kale", "Rogelio", "Casen", "Ray", "Clay", "Masen", "Sage", "Ulises", "Kymani", "Chaim", "Javon", "Brent", "Jadon", "Elisha", "Stanley", "Jovanni", "Princeton", "Alonso", "Darian", "Conrad", "Dwayne", "Eugene", "Gauge", "Rene", "Kareem", "Roland", "Ben", "Vincenzo", "Abdullah", "Camren", "Kenny", "Brentley", "Memphis", "Blaze", "Edison", "Osvaldo", "Teagan", "Westin", "Deshawn", "Rayden", "Cedric", "Marquis", "Samir", "Steve", "Draven", "Jairo", "Giovanny", "Brennen", "Bronson", "Crew", "Davin", "Kolten", "Ronin", "Ariel", "Semaj", "Alden", "Isiah", "Lennox", "Davian", "Jaylin", "Cain", "Wayne", "Craig", "Lamar", "Leonidas", "Cristopher", "Otto", "Bo", "Darrell", "Kolby", "Marcelo", "Bruno", "Fletcher", "Justus", "Alfonso", "Theo", "Tyrone", "Aidyn", "Harvey", "Rudy", "Brendon", "Tristin", "Dominique", "Kaeden", "Samson", "Kyree", "Jovani", "Lionel", "Amos", "Giancarlo", "Misael", "Callum", "Quintin", "Valentino", "Gavyn", "Lennon", "Jamir", "Kamron", "Zavier", "Arlo", "Junior", "Killian", "Leandro", "Konnor", "Hezekiah", "Jordyn", "Markus", "Ramiro", "Callan", "Chace", "Johnathon", "Lyric", "Fisher", "Rashad", "Kamryn", "Legend", "Duncan", "Harold", "Camilo", "Hendrix", "Seamus", "Coleman", "Vance", "Rylee", "Elian", "Jaeden", "Jamie", "Krish", "Abdiel", "Antoine", "Camdyn", "Van", "Branden", "Cayson", "Gibson", "Javion", "Izayah", "Darwin", "Jamar", "Mike", "Randall", "Brecken", "Hassan", "Thiago", "Heath", "Arnav", "Kingsley", "Kyrie", "Xavi", "Damari", "Deangelo", "Jionni", "Joziah", "Makhi", "Vaughn", "Zeke", "Konner", "Ean", "Frankie", "Yael", "Benton", "Oakley", "Efrain", "Marcel", "Rolando", "Maxton", "Jaycob", "Keenan", "Rowen", "Yousef", "Ishaan", "Jedidiah", "Remy", "Todd", "Reagan", "Bodhi", "Damarion", "Juelz", "Valentin", "Austyn", "Broderick", "Anders", "Alvaro", "Mustafa", "Thaddeus", "Brenton", "Cale", "Clinton", "Derick", "Jorden", "Gilberto", "Jabari", "Rey", "Salvatore", "Freddy", "Donte", "Ernest", "Aaden", "Axton", "Blaise", "Lucca", "Maximo", "Sidney", "Dario", "Rodolfo", "Trevon", "Camryn", "Deegan", "Sonny", "Cassius", "Truman", "Brice", "Brogan", "Hugh", "Yehuda", "Agustin", "Eliot", "Stefan", "Zaid", "Bridger", "Damion", "Eliseo", "Houston", "Johann", "Leroy", "Sheldon", "Dariel", "Darryl", "Isai", "Tyrell", "Alfred", "Demarcus", "Kohen", "Ignacio", "Rylen", "Santos", "Cael", "Davon", "Kaysen", "Mack", "Darien", "Ross", "Titan", "Tyree", "Ameer", "Zaire", "Aditya", "Briggs", "Immanuel", "Malaki", "Turner", "Bradyn", "Graysen", "Kase", "Reuben", "Yandel", "Gaige", "Jaidyn", "Franco", "Trystan", "Maison", "Simeon", "Anton", 
  "Darnell", "Emory", "Roderick", "Deon", "Devan", "Graeme", "Howard", "Jael", "Kael", "Karsen", "Jarrett", "Apollo", "Denzel", "Foster", "Gilbert", "Jaylon", "Kylen", "Augustine"
)	

.female.names <- c(
  "Sophia", "Emma", "Isabella", "Olivia", "Ava", "Emily", "Abigail", "Mia", "Madison", "Elizabeth", "Chloe", "Ella", "Avery", "Addison", "Aubrey", "Lily", "Natalie", "Sofia", "Charlotte", "Zoey", "Grace", "Hannah", "Amelia", "Harper", "Lillian", "Samantha", "Evelyn", "Victoria", "Brooklyn", "Zoe", "Layla", "Hailey", "Leah", "Kaylee", "Anna", "Aaliyah", "Gabriella", "Allison", "Nevaeh", "Alexis", "Audrey", "Savannah", "Sarah", "Alyssa", "Claire", "Taylor", "Riley", "Camila", "Arianna", "Ashley", "Brianna", "Sophie", "Peyton", "Bella", "Khloe", "Genesis", "Alexa", "Serenity", "Kylie", "Aubree", "Scarlett", "Stella", "Maya", "Katherine", "Julia", "Lucy", "Madelyn", "Autumn", "Makayla", "Kayla", "Mackenzie", "Lauren", "Gianna", "Ariana", "Faith", "Alexandra", "Melanie", "Sydney", "Bailey", "Caroline", "Naomi", "Morgan", "Kennedy", "Ellie", "Jasmine", "Eva", "Skylar", "Kimberly", "Violet", "Molly", "Aria", "Jocelyn", "Trinity", "London", "Lydia", "Madeline", "Reagan", "Piper", "Andrea", "Annabelle", "Maria", "Brooke", "Payton", "Paisley", "Paige", "Ruby", "Nora", "Mariah", "Rylee", "Lilly", "Brielle", "Jade", "Destiny", "Nicole", "Mila", "Kendall", "Liliana", "Kaitlyn", "Natalia", "Sadie", "Jordyn", "Vanessa", "Mary", "Mya", "Penelope", "Isabelle", "Alice", "Reese", "Gabrielle", "Hadley", "Katelyn", "Angelina", "Rachel", "Isabel", "Eleanor", "Clara", "Brooklynn", "Jessica", "Elena", "Aliyah", "Vivian", "Laila", "Sara", "Amy", "Eliana", "Lyla", "Juliana", "Valeria", "Adriana", "Makenzie", "Elise", "Mckenzie", "Quinn", "Delilah", "Cora", "Kylee", "Rebecca", "Gracie", "Izabella", "Josephine", "Alaina", "Michelle", "Jennifer", "Eden", "Valentina", "Aurora", "Catherine", "Stephanie", "Valerie", "Jayla", "Willow", "Daisy", "Alana", "Melody", "Hazel", "Summer", "Melissa", "Margaret", "Kinsley", "Kinley", "Ariel", "Lila", "Giselle", "Ryleigh", "Haley", "Julianna", "Ivy", "Alivia", "Brynn", "Keira", "Daniela", "Aniyah", "Angela", "Kate", "Londyn", "Hayden", "Harmony", "Adalyn", "Megan", "Allie", "Gabriela", "Alayna", "Presley", "Jenna", "Alexandria", "Ashlyn", "Adrianna", "Jada", "Fiona", "Norah", "Emery", "Maci", "Miranda", "Ximena", "Amaya", "Cecilia", "Ana", "Shelby", "Katie", "Hope", "Callie", "Jordan", "Luna", "Leilani", "Eliza", "Mckenna", "Angel", "Genevieve", "Makenna", "Isla", "Lola", "Danielle", "Chelsea", "Leila", "Tessa", "Adelyn", "Camille", "Mikayla", "Adeline", "Adalynn", "Sienna", "Esther", "Jacqueline", "Emerson", "Arabella", "Maggie", "Athena", "Lucia", "Lexi", "Ayla", "Diana", "Alexia", "Juliet", "Josie", "Allyson", "Addyson", "Delaney", "Teagan", "Marley",
  "Amber", "Rose", "Erin", "Leslie", "Kayleigh", "Amanda", "Kathryn", "Kelsey", "Emilia", "Alina", "Kenzie", "Kaydence", "Alicia", "Alison", "Paris", "Sabrina", "Ashlynn", "Lilliana", "Sierra", "Cassidy", "Laura", "Alondra", "Iris", "Kyla", "Christina", "Carly", "Jillian", "Madilyn", "Kyleigh", "Madeleine", "Cadence", "Nina", "Evangeline", "Nadia", "Raegan", "Lyric", "Giuliana", "Briana", "Georgia", "Yaretzi", "Elliana", "Haylee", "Fatima", "Phoebe", "Selena", "Charlie", "Dakota", "Annabella", "Abby", "Daniella", "Juliette", "Lilah", "Bianca", "Mariana", "Miriam", "Parker", "Veronica", "Gemma", "Noelle", "Cheyenne", "Marissa", "Heaven", "Vivienne", "Brynlee", "Joanna", "Mallory", "Aubrie", "Journey", "Nyla", "Cali", "Tatum", "Carmen", "Gia", "Jazmine", "Heidi", "Miley", "Baylee", "Elaina", "Macy", "Ainsley", "Jane", "Raelynn", "Anastasia", "Adelaide", "Ruth", "Camryn", "Kiara", "Alessandra", "Hanna", "Finley", "Maddison", "Lia", "Bethany", "Karen", "Kelly", "Malia", "Jazmin", "Jayda", "Esmeralda", "Kira", "Lena", "Kamryn", "Kamila", "Karina", "Eloise", "Kara", "Elisa", "Rylie", "Olive", "Nayeli", "Tiffany", "Macie", "Skyler", "Addisyn", "Angelica", "Briella", "Fernanda", "Annie", "Maliyah", "Amiyah", "Jayden", "Charlee", "Caitlyn", "Elle", "Crystal", "Julie", "Imani", "Kendra", "Talia", "Angelique", "Jazlyn", "Guadalupe", "Alejandra", "Emely", "Lucille", "Anya", "April", "Elsie", "Madelynn", "Myla", "Julissa", "Scarlet", "Helen", "Breanna", "Kyra", "Madisyn", "Rosalie", "Brittany", "Brylee", "Jayleen", "Arielle", "Karla", "Kailey", "Arya", "Sarai", "Harley", "Miracle", "Kaelyn", "Kali", "Cynthia", "Daphne", "Aleah", "Caitlin", "Cassandra", "Holly", "Janelle", "Marilyn", "Katelynn", "Kaylie", "Itzel", "Carolina", "Bristol", "Haven", "Michaela", "Monica", "June", "Janiyah", "Camilla", "Jamie", "Rebekah", "Audrina", "Dayana", "Lana", "Serena", "Tiana", "Nylah", "Braelyn", "Savanna", "Skye", "Raelyn", "Madalyn", "Sasha", "Perla", "Bridget", "Aniya", "Rowan", "Logan", "Mckinley", "Averie", "Jaylah", "Aylin", "Joselyn", "Nia", "Hayley", "Lilian", "Adelynn", "Jaliyah", "Kassidy", "Kaylin", "Kadence", "Celeste", "Jaelyn", "Zariah", "Tatiana", "Jimena", "Lilyana", "Anaya", "Catalina", "Viviana", "Cataleya", "Sloane", "Courtney", "Johanna", "Amari", "Melany", "Anabelle", "Francesca", "Ada", "Alanna", "Priscilla", "Danna", "Angie", "Kailyn", "Lacey", "Sage", "Lillie", "Brinley", "Caylee", "Joy", "Kenley", "Vera", "Bailee", "Amira", "Aileen", "Aspen", "Emmalyn", "Erica", "Gracelyn", "Kennedi", "Skyla", "Annalise", "Danica", "Dylan", "Kiley", "Gwendolyn", "Jasmin", "Lauryn", 
  "Aleena", "Justice", "Annabel", "Tenley", "Dahlia", "Gloria", "Lexie", "Lindsey", "Hallie", "Sylvia", "Elyse", "Annika", "Maeve", "Marlee", "Aryanna", "Kenya", "Lorelei", "Selah", "Kaliyah", "Adele", "Natasha", "Brenda", "Erika", "Alyson", "Braylee", "Emilee", "Raven", "Ariella", "Blakely", "Liana", "Jaycee", "Sawyer", "Anahi", "Jaelynn", "Elsa", "Farrah", "Cameron", "Evelynn", "Luciana", "Zara", "Madilynn", "Eve", "Kaia", "Helena", "Anne", "Estrella", "Leighton", "Nataly", "Whitney", "Lainey", "Amara", "Anabella", "Malaysia", "Samara", "Zoie", "Amani", "Phoenix", "Dulce", "Paola", "Marie", "Aisha", "Harlow", "Virginia", "Ember", "Regina", "Jaylee", "Anika", "Ally", "Kayden", "Alani", "Miah", "Yareli", "Journee", "Kiera", "Nathalie", "Mikaela", "Jaylynn", "Litzy", "Charley", "Claudia", "Aliya", "Madyson", "Cecelia", "Liberty", "Braelynn", "Evie", "Rosemary", "Myah", "Lizbeth", "Giana", "Ryan", "Teresa", "Ciara", "Isis", "Lea", "Shayla", "Jazlynn", "Rosa", "Gracelynn", "Desiree", "Elisabeth", "Isabela", "Arely", "Mariam", "Abbigail", "Emersyn", "Brenna", "Kaylynn", "Nova", "Raquel", "Dana", "Laney", "Laylah", "Siena", "Amelie", "Clarissa", "Lilianna", "Lylah", "Halle", "Madalynn", "Maleah", "Sherlyn", "Linda", "Shiloh", "Jessie", "Kenia", "Greta", "Marina", "Melina", "Amiya", "Bria", "Natalee", "Sariah", "Mollie", "Nancy", "Christine", "Felicity", "Zuri", "Irene", "Simone", "Amya", "Matilda", "Colette", "Kristen", "Paityn", "Alayah", "Janiya", 
  "Kallie", "Mira", "Hailee", "Kathleen", "Meredith", "Janessa", "Noemi", "Aiyana", "Aliana", "Leia", "Mariyah", "Tori", "Alissa", "Ivanna", "Joslyn", "Sandra", "Maryam", "Saniyah", "Kassandra", "Danika", "Denise", "Jemma", "River", "Charleigh", "Emelia", "Kristina", "Armani", "Beatrice", "Jaylene", "Karlee", "Blake", "Cara", "Addilyn", "Amina", "Ansley", "Kaitlynn", "Iliana", "Mckayla", "Adelina", "Briley", "Elaine", "Lailah", "Mercedes", "Chaya", "Lindsay", "Hattie", "Lisa", "Marisol", "Patricia", "Bryanna", "Taliyah", "Adrienne", "Emmy", "Millie", "Paislee", "Charli", "Kourtney", "Leyla", "Maia", "Willa", "Milan", "Paula", "Ayleen", "Clare", "Kensley", "Reyna", "Martha", "Adley", "Elianna", "Emilie", "Karsyn", "Yasmin", "Lorelai", "Amirah", "Aryana", "Livia", "Alena", "Kiana", "Celia", "Kailee", "Rylan", "Ellen", "Galilea", "Kynlee", "Leanna", "Renata", "Mae", "Ayanna", "Chanel", "Lesly", "Cindy", "Carla", "Pearl", "Jaylin", "Kimora", "Angeline", "Carlee", "Aubri", "Edith", "Alia", "Frances", "Corinne", "Jocelynn", "Cherish", "Wendy", "Carolyn", "Lina", "Tabitha", "Winter", "Abril", "Bryn", "Jolie", "Yaritza", "Casey", "Zion", "Lillianna", "Jordynn", "Zariyah", "Audriana", "Jayde", "Jaida", "Salma", "Diamond", "Malaya", "Kimber", "Ryann", "Abbie", "Paloma", "Destinee", "Kaleigh", "Asia", "Demi", "Yamileth", "Deborah", "Elin", "Kaiya", "Mara", "Averi", "Nola", "Tara", "Taryn", "Emmalee", "Aubrianna", "Janae", "Kyndall", "Jewel", "Zaniyah", 
  "Kaya", "Sonia", "Alaya", "Heather", "Nathaly", "Shannon", "Ariah", "Avah", "Giada", "Lilith", "Samiyah", "Sharon", "Coraline", "Eileen", "Julianne", "Milania", "Chana", "Regan", "Krystal", "Rihanna", "Sidney", "Hadassah", "Macey", "Mina", "Paulina", "Rayne", "Kaitlin", "Maritza", "Susan", "Raina", "Hana", "Keyla", "Temperance", "Aimee", "Alisson", "Charlize", "Kendal", "Lara", "Roselyn", "Alannah", "Alma", "Dixie", "Larissa", "Patience", "Taraji", "Sky", "Zaria", "Aleigha", "Alyvia", "Aviana", "Bryleigh", "Elliot", "Jenny", "Luz", "Ali", "Alisha", "Ayana", "Campbell", "Karis", "Lilyanna", "Azaria", "Blair", "Micah", "Moriah", "Myra", "Lilia", "Aliza", "Giovanna", "Karissa", "Saniya", "Emory", "Estella", "Juniper", "Kairi", "Kenna", "Meghan", "Abrielle", "Elissa", "Rachael", "Emmaline", "Jolene", "Joyce", "Britney", "Carlie", "Haylie", "Judith", "Renee", "Saanvi", "Yesenia", "Barbara", "Dallas", "Jaqueline", "Karma", "America", "Sariyah", "Azalea", "Everly", "Ingrid", "Lillyana", "Emmalynn", "Marianna", "Brisa", "Kaelynn", "Leona", "Libby", "Deanna", "Mattie", "Miya", "Kai", "Annalee", "Nahla", "Dorothy", "Kaylyn", "Rayna", "Araceli", "Cambria", "Evalyn", "Haleigh", "Thalia", "Jakayla", "Maliah", "Saige", "Avianna", "Charity", "Kaylen", "Raylee", "Tamia", "Aubrielle", "Bayleigh", "Carley", "Kailynn", "Katrina", "Belen", "Karlie", "Natalya", "Alaysia", "Celine", "Milana", "Monroe", "Estelle", "Meadow", "Audrianna", "Cristina", "Harlee", "Jazzlyn", "Scarlette", "Zahra", "Akira", "Ann", "Collins", "Kendyl", "Anabel", "Azariah", "Carissa", "Milena", "Tia", "Alisa", "Bree", "Carleigh", "Cheyanne", "Sarahi", "Laurel", "Kylah", "Tinley", "Kora", "Marisa", "Esme", "Sloan", "Cailyn", "Gisselle", "Kasey", "Kyndal", "Marlene", "Riya", "Annabell", "Aubriana", "Izabelle", "Kirsten", "Aya", "Dalilah", "Devyn", "Geraldine", "Analia", "Hayleigh", "Landry", "Sofie", "Tess", "Ashtyn", "Jessa", "Katalina"
)	

.names <- c(.male.names, .female.names)
.names <- .names[which(!duplicated(.names))]

.opt <- list(
  scdf         = "scdf",
  dv           = "var.values",
  phase        = "var.phase",
  mt           = "var.mt",
  case_name    = "name",
  info         = "info",
  author       = "author",
  female.names = .female.names,
  male.names   = .male.names,
  names        = .names,
  function_debugging_warning  = "This function is in an experimental state and only implemented for testing und debugging purposes.\n",
  function_deprecated_warning = "This function is deprecated. It will be dropped without any further notice in a future update of scan.\n",
  style = list()
  )

.opt$style$default = list(
  frame = "black", annotations = NULL, las = 1, mai = c(0.6, 0.58, 0.2, 0.2), bty = "o", #mai = c(0.6, 0.82, 0.2, 0.42)
  text.ABlag = NULL, pch = 17, font = "sans", ylab.orientation = 0, 
  fill = FALSE, fill.bg = FALSE, grid = FALSE,  
  lwd = 2, lwd.seperators = 1.5, lwd.grid = 1,
  lty = "solid", lty.seperators = "dashed", lty.grid = "dotted",
  cex = 1, cex.axis = 0.8, cex.text = 1, cex.lab = 1,
  col.lines = "black", col.dots = "black", col.seperators = "black", col.fill = "grey75", col.fill.bg = "grey95", 
  col.bg = "white", col = "black", col.text = "black", col.fill.bg = "grey95"
)

.opt$style$yaxis <- list(
  ylab.orientation = 1, mai = c(0.6, 0.85, 0.2, 0.2), cex.lab = 0.8,
  cex.text = 0.8
)

.opt$style$tiny <- list(
  cex.text = 0.5, cex = 0.5, cex.lab = 0.5, 
  lwd = 0.7, lwd.seperators = 0.7, lwd.grid = 0.7,
  mai = c(0.3,0.3,0.1,0.05)
)

.opt$style$small <- list(
  cex.text = 0.75, cex = 0.75, cex.lab = 0.75, 
  lwd = 0.85, lwd.seperators = 0.85, lwd.grid = 0.85,
  mai = c(0.5,0.5,0.15,0.1)
)

.opt$style$big <- list(
  cex.text = 1.25, cex = 1.25, cex.lab = 1.25, 
  lwd = 1.5, lwd.seperators = 1.5, lwd.grid = 1.5,
  mai = c(0.8,1,0.2,0.2)
)



.opt$style$chart <- list(
  fill.bg = TRUE, col.fill.bg = "grey98", fill = TRUE, col.fill = "grey50", 
  annotations = list(cex = 0.6, col = "black", offset = 0.4), pch = 19, 
  frame = NA, grid = "grey75", lwd = 0.7, cex.text = 0.8, cex.lab = 0.8 )


.opt$style$ridge <- list(
  fill = "grey50", fill.bg = TRUE, col.fill.bg = "grey95", pch = 20)

.opt$style$annotate <- list(
  annotations = list(cex = 0.6, col = "black", offset = 0.4), pch = 19)

.opt$style$grid <- list(
  frame = NA, grid = TRUE, col.grid = "lightblue", fill.bg = TRUE, col.fill.bg = "grey95", lwd = 0.7, 
  pch = 19, cex.axis = 0.8)  

.opt$style$grid2 <- list(
  frame = NA, fill = "white", grid = TRUE, col.grid = "lightgreen", frame = "black", 
  fill.bg = TRUE, col.fill.bg = "grey95", lwd = 0.7, pch = 1, cex.axis = 0.8)  

.opt$style$dark <- list(
  fill.bg = TRUE, col.fill.bg = "black", bty = "o", col.lines = "gold", col.bg = "grey10", 
  col.dots = "red", col.seperators = "white", col = "white", 
  col.text = "white")

.opt$style$nodots <- list(
  type = "l", col.dots = "", fill = TRUE, col.fill = "grey95", grid = TRUE, col.grid = "grey80",
  fill.bg = TRUE, col.fill.bg = "grey99")

.opt$style$sienna <- list(
  grid = TRUE, col.grid = "orange", pch = 18, col.lines = "grey85", col.dots = "seagreen4", 
  lwd = 2, col.bg = "seashell", fill.bg = "moccasin", col.text = "sienna4", 
  col = "darkolivegreen", col.seperators = "sienna4", 
  cex.text = 0.8, cex.lab = 0.8, cex.axis = 0.7, frame = "darkseagreen", 
  font = "serif")


.opt$mc_fun <- list(
  plm_level = function(x) .plm.mt(x, type = "level p"),
  plm_slope = function(x) .plm.mt(x, type = "slope p"),
  plm_poisson_level = function(x) .plm.mt(x, count.data = TRUE, type = "level p"),
  plm_poisson_slope = function(x) .plm.mt(x, count.data = TRUE, type = "slope p"),
  hplm_level = function(x, ...) {
      res <- summary(hplm(x, random.slopes = FALSE, ICC = FALSE, ...)$hplm)$tTable
      #param <- (nrow(res) - 2) / 2
      res[3, 5]
    },
  hplm_slope = function(x, ...) {
    res <- summary(hplm(x, random.slopes = FALSE, ICC = FALSE, ...)$hplm)$tTable
    param <- (nrow(res) - 2) / 2
    res[2 + param + 1, 5]
  },
  tauU = function(x) tauUSC(x, method = "parker")$table[[1]][5, 12],
  base_tau = function(x) corrected_tauSC(x)$p,
  
  rand = function(x) randSC(x, number = 100, exclude.equal = "auto", output = "p")
)
