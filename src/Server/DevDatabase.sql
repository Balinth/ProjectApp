CREATE TABLE IF NOT EXISTS Project(
    Code text NOT NULL UNIQUE,
    Name text NOT NULL UNIQUE,
    ID int PRIMARY KEY,
    StartDate int
);
CREATE TABLE IF NOT EXISTS Structure(
    ID int PRIMARY KEY,
    Name text NOT NULL UNIQUE,
    Project_ID int,
    FOREIGN KEY (Project_ID)
       REFERENCES Project (Project_ID) 
);
CREAABLE IF NOT EXISTS User(
    ID int PRIMARY KEY,
    Name text NOT NULL UNIQUE,
    PrimaryEmail text NOT NULL UNIQUE,
);