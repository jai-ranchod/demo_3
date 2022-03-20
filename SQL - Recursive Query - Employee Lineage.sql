/*
	In this example, we have a simple table of managers and employees.  The objective is to create a table where 
	we see the IDs of top managers and the number of employees who report directly or indirectly to that manager.
	We define "top manager" as a manager with no manager ID specified. 

	The general idea is this:
		1. Order the table by manager ID, then assign row numbers.  This ensures
			that all entries  will have sequential integers assigned to their row in the table we call "new", 
			with the lowest integers in the rows where there is NULL in the "managerId" field (by definition top level
			managers).
		2.  Create a table to hold the row ID and count of employees for top level managers.  This will be used as a kind of
			"staging table" later on.
		3.  Determine how many top level managers we have, and store that number in a variable we can employee in our while loop.
			Also, create a variable to keep track of the current row ID in the while loop structure.
		4.  Create recursive query structure that counts all of the employees in the "lineage" of our top level managers
			one at a time.  
		5.	Embed our recursive structure in a while loop, running it for each top level manager we have identified.
		6.	Using the table built by our recursive query, connect to our original "new" table (built in step 1) and identify
			each manager and the number of employees that report to that manager.


*/

--Creating Table to use in our example
CREATE TABLE
employees(
id INT
,name VARCHAR(32)
,managerId INT
)
INSERT INTO employees(id, name, managerID)
VALUES(12,'Rob',NULL)
INSERT INTO employees(id, name, managerID)
VALUES(25,'Tom',NULL)
INSERT INTO employees(id, name, managerID)
VALUES(13,'Mark',25)
INSERT INTO employees(id, name, managerID)
VALUES(4,'Jim',12)
INSERT INTO employees(id, name, managerID)
VALUES(50,'Lisa',4)
INSERT INTO employees(id, name, managerID)
VALUES(6,'Bill',50)
INSERT INTO employees(id, name, managerID)
VALUES(7,'Taylor',13)

--Step 1:Assigning sequential integers to rows
CREATE TABLE new
(
	id INT
	,name VARCHAR(32)
	,managerID INT
	,RowId INT
)
INSERT INTO new 
SELECT
*
,ROW_NUMBER() OVER (ORDER BY managerID) AS RowId
FROM
employees
ORDER BY managerId

--Step 2: Create table to hold row ID and employee count for top level managers
CREATE TABLE ManagementCount(
RowID INT,
NumberOfReports INT
)

--Step 3: Create variables to track the number of top level managers and our progress through our loop
DECLARE @managers INT = (SELECT COUNT(*) FROM new WHERE managerID IS NULL)
DECLARE @rid INT = 1;

--Step 5: (Step 4 Below) Create WHILE loop in which to embed our recursive query
WHILE @rid <= @managers
BEGIN
--Step 4: Create recursive query to identify number of employees under each top manager
WITH subordinate AS (
    SELECT  id,
            name,
            managerId,
            0 AS level
    FROM new
    WHERE RowId = @rid
 
    UNION ALL
 
    SELECT  n.id,
            n.name,
            n.managerId,
            level + 1
    FROM new n
JOIN subordinate s
ON n.managerId = s.id
)
--This is where we insert the row ID and employee count of top managers individually
INSERT INTO ManagementCount (RowID, NumberOfReports)
SELECT
@rid as ID
	,(SELECT 
	COUNT(*)
	FROM subordinate s
	JOIN new m
	ON s.managerId = m.id) as EmployeeCount

SET @rid = @rid + 1
END;
--Step 6: Connecting tables to identify managers and employee counts
SELECT 
id as ManagerId
,N.name as ManagerName 
,NumberOfReports
FROM ManagementCount MC
	INNER JOIN new N
	on MC.RowID = N.RowID
DROP TABLE ManagementCount
DROP TABLE new
DROP TABLE employees


