CREATE TABLE IF NOT EXISTS Project(
    ProjectID INTEGER PRIMARY KEY,
    ProjectName TEXT NOT NULL UNIQUE,
    Code TEXT NOT NULL UNIQUE,
    StartDate INTEGER NOT NULL
);
CREATE TABLE IF NOT EXISTS Phase(
    PhaseID INTEGER NOT NULL,
    PhaseName TEXT NOT NULL UNIQUE,
    Revision TEXT NOT NULL UNIQUE,
    Project_ID INTEGER NOT NULL,
    ForFabrication_ID INTEGER NOT NULL,
    Mass REAL NOT NULL,
    FOREIGN KEY (Project_ID)
       REFERENCES Project (ProjectID),
    FOREIGN KEY (ForFabrication_ID)
        REFERENCES ForFabrication (ForFabrication_ID)
    PRIMARY KEY (PhaseID, Revision, Project_ID)
);
CREATE TABLE IF NOT EXISTS User(
    UserID INTEGER PRIMARY KEY,
    UserName TEXT NOT NULL UNIQUE,
    GivenName TEXT NOT NULL,
    FamilyName TEXT NOT NULL,
    PrimaryEmail TEXT NOT NULL UNIQUE,
    UserNameID TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS Organization(
    OrganizationID INTEGER PRIMARY KEY,
    OrganizationName TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS OrganizationMember(
    User_ID INTEGER NOT NULL,
    Organization_ID INTEGER NOT NULL,
    FromDate INTEGER NOT NULL,
    ToDate INTEGER NOT NULL,
    FOREIGN KEY (User_ID)
        REFERENCES User (UserID),
    FOREIGN KEY (Organization_ID)
        REFERENCES Organization (OrganizationID),
    PRIMARY KEY (User_ID, Organization_ID, FromDate, ToDate)
);

CREATE TABLE IF NOT EXISTS ForFabrication(
    ForFabricationID INTEGER PRIMARY KEY,
    SenderUser_ID INTEGER NOT NULL,
    EmailOriginal TEXT NOT NULL,
    SentDate INTEGER NOT NULL,
    FOREIGN KEY (SenderUser_ID)
        REFERENCES User (UserID)
);

CREATE TABLE IF NOT EXISTS Designer(
    User_ID INTEGER NOT NULL,
    Phase_ID INTEGER NOT NULL,
    Phase_ProjectID INTEGER NOT NULL,
    Phase_Revision TEXT NOT NULL,
    FOREIGN KEY (User_ID)
        REFERENCES User (UserID),
    FOREIGN KEY (Phase_ID)
        REFERENCES Phaes (PhaseID),
    FOREIGN KEY (Phase_ProjectID)
        REFERENCES Phase (Project_ID),
    FOREIGN KEY (Phase_Revision)
        REFERENCES Phase (Revision),
    PRIMARY KEY (User_ID, Phase_ID, Phase_ProjectID, Phase_Revision)
);

CREATE TABLE IF NOT EXISTS Drafter(
    User_ID INTEGER NOT NULL,
    Phase_ID INTEGER NOT NULL,
    Phase_ProjectID INTEGER NOT NULL,
    Phase_Revision TEXT NOT NULL,
    FOREIGN KEY (User_ID)
        REFERENCES User (UserID),
    FOREIGN KEY (Phase_ID)
        REFERENCES Phaes (PhaseID),
    FOREIGN KEY (Phase_ProjectID)
        REFERENCES Phase (Project_ID),
    FOREIGN KEY (Phase_Revision)
        REFERENCES Phase (Revision),
    PRIMARY KEY (User_ID, Phase_ID, Phase_ProjectID, Phase_Revision)
);

CREATE TABLE IF NOT EXISTS LocalAuthentication(
    LocalAuthID INTEGER PRIMARY KEY,
    PasswordHash TEXT NOT NULL,
    Salt TEXT NOT NULL,
    User_ID INTEGER,
    FOREIGN KEY (User_ID)
        REFERENCES User (UserID)
);
