CREATE TABLE IF NOT EXISTS Project(
    ProjectName text NOT NULL UNIQUE,
    ProjectID int PRIMARY KEY,
    Code text NOT NULL UNIQUE,
    StartDate int
);
CREATE TABLE IF NOT EXISTS Phase(
    PhaseID int PRIMARY KEY,
    PhaseName text NOT NULL UNIQUE,
    Project_ID int,
    FOREIGN KEY (Project_ID)
       REFERENCES Project (ProjectID) 
);
CREATE TABLE IF NOT EXISTS User(
    UserID int PRIMARY KEY,
    UserName text NOT NULL UNIQUE,
    PrimaryEmail text NOT NULL UNIQUE,
    UserNameID text NOT NULL UNIQUE
);