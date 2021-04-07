CREATE TABLE IF NOT EXISTS Project(
    ProjectID int PRIMARY KEY,
    ProjectName text NOT NULL UNIQUE,
    Code text NOT NULL UNIQUE,
    StartDate int
);
CREATE TABLE IF NOT EXISTS Phase(
    PhaseID int,
    PhaseName text NOT NULL UNIQUE,
    Revision text NOT NULL UNIQUE,
    Project_ID int,
    ForFabrication_ID int,
    PRIMARY KEY (PhaseID, Revision),
    FOREIGN KEY (Project_ID)
       REFERENCES Project (ProjectID),
    FOREIGN KEY (ForFabrication_ID)
        REFERENCES ForFabrication (ForFabrication_ID)
);
CREATE TABLE IF NOT EXISTS User(
    UserID int PRIMARY KEY,
    UserName text NOT NULL UNIQUE,
    GivenName text NOT NULL,
    FamilyName text NOT NULL,
    PrimaryEmail text NOT NULL UNIQUE,
    UserNameID text NOT NULL UNIQUE,
);
CREATE TABLE IF NOT EXISTS ForFabrication(
    ForFabricationID int PRIMARY KEY,
    SenderUser_ID int,
    EmailOriginal text NOT NULL,
    SentDate int,
    FOREIGN KEY (SenderUser_ID)
        REFERENCES User (UserID),
);
CREATE TABLE IF NOT EXISTS LocalAuthentication(
    LocalAuthID int PRIMARY KEY,
    PasswordHash text NOT NULL,
    Salt text NOT NULL,
    User_ID int,
    FOREIGN KEY (UserID)
        REFERENCES User (UserID)
);
