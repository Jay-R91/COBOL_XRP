IDENTIFICATION DIVISION.
PROGRAM-ID. XRP-PROCESSOR.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT BALANCE-FILE ASSIGN TO "balance.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL
        FILE STATUS IS FILE-STATUS.

DATA DIVISION.
FILE SECTION.
FD BALANCE-FILE.
01 BALANCE-RECORD PIC X(7).  *> Store balance as a 7-character string

WORKING-STORAGE SECTION.
01 FILE-STATUS          PIC X(2).
01 WS-BALANCE           PIC 9(5)V99.  *> Balance with 2 decimal places
01 WS-BALANCE-INT       PIC 9(7).     *> Integer balance in cents
01 WS-BALANCE-EDIT      PIC 0000000.  *> 7-digit string with leading zeros
01 WS-DISPLAY-BALANCE   PIC $Z,ZZZ,ZZ9.99.  *> For display purposes
01 WS-RAW-BALANCE       PIC X(7).     *> Raw file input
01 SENDER               PIC X(20).    *> Sender name
01 RECEIVER             PIC X(20).    *> Receiver name
01 AMOUNT               PIC 9(5)V99.  *> Transaction amount

PROCEDURE DIVISION.
MAIN-PROCEDURE SECTION.
    PERFORM INITIALIZE-BALANCE
    PERFORM GET-INPUT
    PERFORM PROCESS-TRANSACTION
    STOP RUN.

INITIALIZE-BALANCE SECTION.
    OPEN INPUT BALANCE-FILE
    IF FILE-STATUS = "35"  *> File not found
        CLOSE BALANCE-FILE
        OPEN OUTPUT BALANCE-FILE
        MOVE 1000000 TO WS-BALANCE-INT  *> Default $10,000.00 as 1000000 cents
        MOVE WS-BALANCE-INT TO WS-BALANCE-EDIT
        MOVE WS-BALANCE-EDIT TO BALANCE-RECORD
        WRITE BALANCE-RECORD
        CLOSE BALANCE-FILE
    ELSE
        CLOSE BALANCE-FILE
    END-IF.

GET-INPUT SECTION.
    OPEN INPUT BALANCE-FILE
    IF FILE-STATUS = "00"
        READ BALANCE-FILE INTO WS-RAW-BALANCE
            AT END
                DISPLAY "File empty, using default balance."
                MOVE 1000000 TO WS-BALANCE-INT
            NOT AT END
                MOVE FUNCTION NUMVAL(WS-RAW-BALANCE) TO WS-BALANCE-INT
                COMPUTE WS-BALANCE = WS-BALANCE-INT / 100
                MOVE WS-BALANCE TO WS-DISPLAY-BALANCE
                DISPLAY "Read balance: " WS-DISPLAY-BALANCE
            END-READ
        CLOSE BALANCE-FILE
    ELSE
        DISPLAY "Failed to open balance.dat. Status: " FILE-STATUS
        MOVE 10000.00 TO WS-BALANCE
    END-IF.
    DISPLAY "Current Balance: " WS-DISPLAY-BALANCE
    DISPLAY "Enter sender name: "
    ACCEPT SENDER
    DISPLAY "Enter receiver name: "
    ACCEPT RECEIVER
    DISPLAY "Enter amount to send: "
    ACCEPT AMOUNT.

PROCESS-TRANSACTION SECTION.
    IF AMOUNT IS NUMERIC
        IF AMOUNT > WS-BALANCE
            DISPLAY "Insufficient funds. Transaction cancelled."
        ELSE
            COMPUTE WS-BALANCE = WS-BALANCE - AMOUNT
            MOVE WS-BALANCE TO WS-DISPLAY-BALANCE
            DISPLAY "Processing transaction..."
            DISPLAY "From: " SENDER
            DISPLAY "To: " RECEIVER
            MOVE AMOUNT TO WS-DISPLAY-BALANCE
            DISPLAY "Amount: " WS-DISPLAY-BALANCE
            MOVE WS-BALANCE TO WS-DISPLAY-BALANCE
            DISPLAY "New Balance: " WS-DISPLAY-BALANCE
            COMPUTE WS-BALANCE-INT = WS-BALANCE * 100  *> Convert to cents
            MOVE WS-BALANCE-INT TO WS-BALANCE-EDIT     *> Format as 7-digit string
            MOVE WS-BALANCE-EDIT TO BALANCE-RECORD      *> Prepare record
            CLOSE BALANCE-FILE
            OPEN OUTPUT BALANCE-FILE                    *> Overwrite file
            WRITE BALANCE-RECORD
            CLOSE BALANCE-FILE
            DISPLAY "Transaction sent successfully!"
        END-IF
    ELSE
        DISPLAY "Invalid amount entered. Must be a number."
    END-IF.

END PROGRAM XRP-PROCESSOR.
