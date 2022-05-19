--Overview--
/*
This file shows selected problems from my Advanced Topcis SQL Server class.  It is based on the "Adventureworks 2019" dataset
that can be found here:
https://docs.microsoft.com/en-us/sql/samples/adventureworks-install-configure?view=sql-server-ver15&tabs=ssms
*/
--Window Functions --

/*
Create a query that shows product name, category, subcategory, and list price
as well as a ranking by list price.  Then add a category ranking and a string indicating
whether or not the product is in the top 5 of list prices in its respective category

*/

SELECT
p.Name as ProductName
,p.ListPrice
,s.Name as ProductSubcategory
,c.Name as ProductCategory
,PriceRank = ROW_NUMBER() OVER(ORDER BY ListPrice DESC)
,CategoryRank = ROW_NUMBER() OVER(PARTITION BY c.Name ORDER BY ListPrice DESC)
,CASE
	WHEN DENSE_RANK() OVER(PARTITION BY c.Name ORDER BY ListPrice DESC) <= 5 THEN 'Yes'
	WHEN DENSE_RANK() OVER(PARTITION BY c.Name ORDER BY ListPrice DESC) >5 THEN 'No'
END as Top5Rank
,CategoryPriceRankWithRank = RANK() OVER(PARTITION BY c.Name ORDER BY ListPrice DESC)
,CategoryPriceRankWithDenseRank = DENSE_RANK() OVER(PARTITION BY c.Name ORDER BY ListPrice DESC)
FROM Production.Product p 
	INNER JOIN Production.ProductSubcategory s
	on p.ProductSubcategoryID = s.ProductSubcategoryID
		INNER JOIN Production.ProductCategory c
		on c.ProductCategoryID = s.ProductCategoryID
ORDER BY ListPrice DESC
----------------------------------------------------------------
----------------------------------------------------------------
/*
Create a query showing the purchase order ID, Order Date, total due,
vendor name, and employee ID associated with purchasing orders.  Totals must be 
greater than $500 and orders must be after 2013.

Then add a column specifying the previous total due with the set of orders 
by vendor ID. 
Then add a column specifying the second next vendor name within the set of orders associated
with a given employee ID.

*/

SELECT
h.PurchaseOrderID
,h.OrderDate
,h.TotalDue
,v.Name as VendorName
,h.VendorID
,h.EmployeeID
,PrevOrderFromVendorAmt = LAG(TotalDue,1) OVER(PARTITION BY h.VendorID ORDER BY h.OrderDate)
,NextOrderByEmployeeVendor = LEAD(v.Name,2) OVER(PARTITION BY h.EmployeeID ORDER BY h.OrderDate)
FROM Purchasing.PurchaseOrderHeader h INNER JOIN Purchasing.Vendor v
on h.VendorID = v.BusinessEntityID
WHERE
YEAR(OrderDate) >= 2013
AND TotalDue > 500
ORDER BY EmployeeID, OrderDate
----------------------------------------------------------------
----------------------------------------------------------------

--Subqueries--

/*
Write a query that outputs the following columns from the PurchaseOrderHeader table:
Purchase Order ID, Vendor ID, OrderDate, TotalDue.

Also add a derived column that returns the number of line items from the asociated orders
in the PurchaseOrderDetail table that do no have any rejections.

Add another derived column holding the most expensive line item of the associated 
order from the PurchaseOrderDetail table.

*/

SELECT
PurchaseOrderID
,VendorID
,OrderDate
,TotalDue
,NonRejectedItems = 
(
	SELECT
	COUNT(*)
	FROM
	Purchasing.PurchaseOrderDetail x
	WHERE x.PurchaseOrderID = y.PurchaseOrderID
	AND RejectedQty = 0
)
,MostExpensiveItem = 
(
	SELECT
	MAX(w.UnitPrice)
	FROM
	Purchasing.PurchaseOrderDetail w
	WHERE w.PurchaseOrderID = y.PurchaseOrderID
)

FROM
Purchasing.PurchaseOrderHeader y
----------------------------------------------------------------
----------------------------------------------------------------
/*
Create a query that displays all rows from the PRoduction.ProductSubcategory
table, and and alias the "name" column as "SubcategoryName".
Add a derived field called "products" which displays, for each Subcategory in 
Production.ProductSubcategory, a semi-colon separated 
list of all products from Production.Product contained within the given 
subcategory.

Filter your query such that only products with a list price greater than $50
are included in the "Products" field.
*/

DECLARE @ListPriceLimit numeric = 50
SELECT
ProductSubcategoryID
,ProductCategoryID
,Name as SubCategoryName
,rowguid
,ModifiedDate
,Products = 
			STUFF(
				(SELECT
						';' + Name -- concatonating line totals with a comma, this is where you pick the delimiter; extra cast is to get rid of trailing zeros
						FROM
						Production.Product x
						WHERE x.ProductSubcategoryID = y.ProductSubcategoryID -- correlated subquery component
						AND x.ListPrice > @ListPriceLimit
						FOR XML PATH('')),
						1,1,''
				  )
FROM
Production.ProductSubcategory y


----------------------------------------------------------------
----------------------------------------------------------------
/*
Write a query against the HumanResources.Employee table that summarizes teh average
amount of vacation time for sales representatives, buyers, and janitors.

Stratify the results by gender.
*/

SELECT
*
FROM
(SELECT
JobTitle
,VacationHours
,Gender as [Employee Gender]
FROM
HumanResources.Employee
) x

PIVOT(
AVG(VacationHours) -- aggregate function over data column
FOR JobTitle IN([Sales Representative], [Buyer], [Janitor]) -- Column to refer to for data headers and each of the data headers desired spelled out
) y
----------------------------------------------------------------
----------------------------------------------------------------
/*
Select all records from the Purchasing.PurchaseOrderHeader table such that
NONE of the items within the order have a rejected quantity greater than 0.


Included options via commenting code that allow a programmer to filter on 
order quantity and unit price of at least one item in the associated orders.
*/

DECLARE @LimitQty numeric = 500
DECLARE @LimitUnitPrice numeric = 50
DECLARE @LimitRejectedQty numeric = 0

SELECT
x.*
FROM
Purchasing.PurchaseOrderHeader x
WHERE NOT EXISTS
	(
		SELECT	
		1
		FROM
		Purchasing.PurchaseOrderDetail y
		WHERE y.PurchaseOrderID = x.PurchaseOrderID
		--AND y.OrderQty > @LimitQty
		--AND y.UnitPrice > @LimitUnitPrice
		AND y.RejectedQty > @LimitRejectedQty
	)
ORDER BY x.PurchaseOrderID
----------------------------------------------------------------
----------------------------------------------------------------

--CTE (Common Table Expressions)--

/*
The CEO of our fictional company decided that the top 10 orders per month are actually outliers that need to be clipped
out of our data before doing meaningful analysis.

Further, she would like the sum of sales AND purchases (minus these "outliers") listed side by side, by month.
Also include prior month sales and purchase figures alongside the current month data.

*/
with PurchaseTable AS
(SELECT
	OrderMonth
	,TotalPurchases = SUM(Purchases)
	FROM
	(SELECT
	*
	FROM
		(SELECT
		OrderMonth = DATEFROMPARTS(YEAR(OrderDate),MONTH(OrderDate),1)
		,TotalDue as Purchases
		,PurchasesRank = ROW_NUMBER() OVER(PARTITION BY DATEFROMPARTS(YEAR(OrderDate),MONTH(OrderDate),1) ORDER BY TotalDue DESC)
		FROM
		Purchasing.PurchaseOrderHeader) x
	WHERE PurchasesRank > 10) y
	GROUP BY OrderMonth),

SalesTable AS
(SELECT
	OrderMonth
	,TotalSales = SUM(Sales)
	FROM
	(SELECT
	*
	FROM
		(SELECT
		OrderMonth = DATEFROMPARTS(YEAR(OrderDate),MONTH(OrderDate),1)
		,TotalDue as Sales
		--,SubTotal as Purchases
		,SalesRank = ROW_NUMBER() OVER(PARTITION BY DATEFROMPARTS(YEAR(OrderDate),MONTH(OrderDate),1) ORDER BY TotalDue DESC)
		FROM
		Sales.SalesOrderHeader) x
	WHERE SalesRank > 10) y
	GROUP BY OrderMonth
)

SELECT
SalesTable.OrderMonth
,SalesTable.TotalSales
,PriorMonthSales = LAG(SalesTable.TotalSales, 1) OVER(ORDER BY SalesTable.OrderMonth)
,PurchaseTable.TotalPurchases
,PriorMonthPurchase = LAG(PurchaseTable.TotalPurchases, 1) OVER(ORDER BY SalesTable.OrderMonth)
FROM
SalesTable 
INNER JOIN
PurchaseTable
on SalesTable.OrderMonth = PurchaseTable.OrderMonth
ORDER BY SalesTable.OrderMonth
----------------------------------------------------------------
----------------------------------------------------------------

/*
Use a recursive CTE to generate a date 
series of all FIRST days of the month (1/1/2021, 2/1/2021, etc.)
from 1/1/2020 to 12/1/2029.
*/

WITH DateSeries AS
(
	SELECT CAST('01-01-2020' AS DATE) AS MyDate
	UNION ALL
	
	SELECT
	DATEADD(MONTH, 1, MyDate)
	FROM
	DateSeries
	WHERE MyDate < CAST('12-01-2029' AS DATE)
)

SELECT
MyDate
FROM
DateSeries
OPTION(MAXRECURSION 365)
----------------------------------------------------------------
----------------------------------------------------------------

--Temporary Tables--

/*
Create a table to hold  sales orders.  The fields should include
Sales Order ID
Order Date
Tax Amount
Freight Cost
Total Due
Percentage of Tax Plus Freight (based on total due)
Classification of tax and frieght cost as 'small' (<10%), 'medium' (<20%), or 'large'(>=20%)
Classification of total due cost as 'small' (<100), 'medium' (<1000), or 'large' (>=1000)


Classify all Q1,2,3 orders as 'non-holiday'
Classify all Q4 orders as 'holiday'

Classify the subcategory as a concatonation of order category and order amount classification
*/

CREATE TABLE #SalesOrders
(
 SalesOrderID INT,
 OrderDate DATE,
 TaxAmt MONEY,
 Freight MONEY,
 TotalDue MONEY,
 TaxFreightPercent FLOAT,
 TaxFreightBucket VARCHAR(32),
 OrderAmtBucket VARCHAR(32),
 OrderCategory VARCHAR(32),
 OrderSubcategory VARCHAR(32)
)

INSERT INTO #SalesOrders
(
 SalesOrderID,
 OrderDate,
 TaxAmt,
 Freight,
 TotalDue,
 OrderCategory
)

SELECT
 SalesOrderID,
 OrderDate,
 TaxAmt,
 Freight,
 TotalDue,
 OrderCategory = 'Non-holiday Order'

FROM [AdventureWorks2019].[Sales].[SalesOrderHeader]

WHERE YEAR(OrderDate) = 2013


UPDATE #SalesOrders
SET 
TaxFreightPercent = (TaxAmt + Freight)/TotalDue,
OrderAmtBucket = 
	CASE
		WHEN TotalDue < 100 THEN 'Small'
		WHEN TotalDue < 1000 THEN 'Medium'
		ELSE 'Large'
	END


UPDATE #SalesOrders
SET TaxFreightBucket = 
	CASE
		WHEN TaxFreightPercent < 0.1 THEN 'Small'
		WHEN TaxFreightPercent < 0.2 THEN 'Medium'
		ELSE 'Large'
	END


UPDATE #SalesOrders
SET  OrderCategory = 'Holiday'
FROM #SalesOrders
WHERE DATEPART(quarter,OrderDate) = 4


UPDATE #SalesOrders
SET OrderSubcategory = CONCAT(OrderCategory,' - ',OrderAmtBucket)

SELECT * FROM #SalesOrders

DROP TABLE #SalesOrders

----------------------------------------------------------------
----------------------------------------------------------------

--Optimization--

/*
Re-write the given initial query using temp tables, update statements, and added indeces.

*/

--Initial Query--
SELECT 
	   A.BusinessEntityID
      ,A.Title
      ,A.FirstName
      ,A.MiddleName
      ,A.LastName
	  ,B.PhoneNumber
	  ,PhoneNumberType = C.Name
	  ,D.EmailAddress

FROM AdventureWorks2019.Person.Person A
	LEFT JOIN AdventureWorks2019.Person.PersonPhone B
		ON A.BusinessEntityID = B.BusinessEntityID
	LEFT JOIN AdventureWorks2019.Person.PhoneNumberType C
		ON B.PhoneNumberTypeID = C.PhoneNumberTypeID
	LEFT JOIN AdventureWorks2019.Person.EmailAddress D
		ON A.BusinessEntityID = D.BusinessEntityID
--re-written as:


--1. Create initial table that gets you the most fields right off the bat
DROP TABLE #PersonContactInfo
CREATE TABLE #PersonContactInfo
(
	   BusinessEntityID INT
      ,Title VARCHAR(8)
      ,FirstName VARCHAR(50)
      ,MiddleName VARCHAR(50)
      ,LastName VARCHAR(50)
	  ,PhoneNumber VARCHAR(25)
	  ,PhoneNumberTypeID VARCHAR(25)
	  ,PhoneNumberType VARCHAR(25)
	  ,EmailAddress VARCHAR(50)
)

--2. Fill your initial table
INSERT INTO #PersonContactInfo
(
	   BusinessEntityID
      ,Title
      ,FirstName
      ,MiddleName
      ,LastName
)

SELECT
	   BusinessEntityID
      ,Title
      ,FirstName
      ,MiddleName
      ,LastName

FROM AdventureWorks2019.Person.Person

CREATE CLUSTERED INDEX Contact_idx ON #PersonContactInfo(BusinessEntityID)



--3. Run a sequence of updates to fill in the gaps
UPDATE A
SET
	PhoneNumber = B.PhoneNumber,
	PhoneNumberTypeID = B.PhoneNumberTypeID

FROM #PersonContactInfo A
	JOIN AdventureWorks2019.Person.PersonPhone B
		ON A.BusinessEntityID = B.BusinessEntityID


CREATE NONCLUSTERED INDEX PhoneType_idx ON #PersonContactInfo(PhoneNumberTypeID)


UPDATE A
SET	PhoneNumberType = B.Name

FROM #PersonContactInfo A
	JOIN AdventureWorks2019.Person.PhoneNumberType B
		ON A.PhoneNumberTypeID = B.PhoneNumberTypeID


UPDATE A
SET	EmailAddress = B.EmailAddress

FROM #PersonContactInfo A
	JOIN AdventureWorks2019.Person.EmailAddress B
		ON A.BusinessEntityID = B.BusinessEntityID


SELECT * FROM #PersonContactInfo
----------------------------------------------------------------
----------------------------------------------------------------

/*
Create a lookup table of dates between January 1, 2011 and December 31, 2030.
Include data such as:

Day of week number (1-7)
Day of week name (Monday, Tuesday, etc.)
Day of month number (1-30ish)
Month number (1-12)
Year number (i.e. 2012)
Flag for weekend Y/N
Flag for holiday Y/N

Use this table to return data about all purchase orders place on a holiday that is also a weekend

*/
CREATE TABLE AdventureWorks2019.dbo.Calendar
(
	DateValue DATE,
	DayOfWeekNumber INT,
	DayOfWeekName VARCHAR(32),
	DayOfMonthNumber INT,
	MonthNumber INT,
	YearNumber INT,
	WeekendFlag TINYINT,
	HolidayFlag TINYINT

);

WITH Dates AS
(
SELECT CAST('01-01-2011' as date) AS MyDate

UNION ALL

SELECT
DATEADD(DAY, 1, MyDate)
FROM Dates
WHERE MyDate < CAST('12-31-2030' as date)
)

INSERT INTO AdventureWorks2019.dbo.Calendar( --adding insert into traditional
-- recursive table structure
DateValue
)


SELECT
MyDate
FROM
Dates
OPTION (MAXRECURSION 10000)


--Adding values to the rest of the columns
/*
()
*
#
*/

UPDATE AdventureWorks2019.dbo.Calendar
SET
DayOfWeekNumber = DATEPART(WEEKDAY, DateValue),
DayOfWeekName = FORMAT(DateValue, 'dddd'),
DayOfMonthNumber = DAY(DateValue),
MonthNumber = MONTH(DateValue),
YearNumber = YEAR(DateValue)

--Adding Weekend Flag and Holiday Flag

UPDATE AdventureWorks2019.dbo.Calendar
SET
WeekendFlag = 
CASE WHEN DayOfWeekName IN ('Saturday', 'Sunday') THEN 1
	ELSE 0
END

--Adding Holiday Flag For only New Years

UPDATE AdventureWorks2019.dbo.Calendar
SET
HolidayFlag = 
CASE WHEN DayOfMonthNumber = 1 AND MonthNumber = 1 THEN 1 -- New Years
		WHEN DayOfMonthNumber = 25 AND MonthNumber = 12 THEN 1 -- Christmas
		WHEN DayOfMonthNumber = 4 AND MonthNumber = 7 THEN 1 -- Independence Day
		WHEN DayOfMonthNumber = 11 AND MonthNumber = 11 THEN 1 -- Veterans Day
		WHEN DayOfMonthNumber = 31 AND MonthNumber = 12 THEN 1 -- New Years Eve
		WHEN DayOfMonthNumber = 17 AND MonthNumber = 3 THEN 1 -- St. Patricks Day
		WHEN DayOfMonthNumber = 14 AND MonthNumber = 2 THEN 1 -- St. Valentines Day
		WHEN DayOfMonthNumber = 2 AND MonthNumber = 2 THEN 1 -- St. Groundhog Day
		WHEN DayOfMonthNumber = 5 AND MonthNumber = 5 THEN 1 -- Cinco de Mayo
		WHEN DayOfMonthNumber = 1 AND MonthNumber = 4 THEN 1 -- April Fools Day
		WHEN DayOfMonthNumber = 19 AND MonthNumber = 7 THEN 1 -- Made Up Holiday - doesn't exist but I needed one that was on a weekend


	ELSE 0
END


--Pull All Purchasing Orders that were made on a holiday that also fell on a weekend:
SELECT
A.*
FROM
Purchasing.PurchaseOrderHeader A
	INNER JOIN AdventureWorks2019.dbo.Calendar B
	on A.OrderDate = B.DateValue
WHERE 1=1
AND B.HolidayFlag = 1
AND B.WeekendFlag = 1

DROP TABLE AdventureWorks2019.dbo.Calendar

----------------------------------------------------------------
----------------------------------------------------------------

--Programming in SQL--

/*
Create a stored procedure to return data about all sales orders above a user defined threshold that take place
between two user specified years
*/


USE AdventureWorks2019
GO -- wrap stored procedure in "go" if you need to run it all in one block
CREATE PROCEDURE dbo.OrdersAboveThreshold(@Threshold INT, @StartYear INT, @EndYear INT) -- Creating procedure

AS -- part of procedure definition

BEGIN -- beginning stored procedure

SELECT
*
FROM
Sales.SalesOrderHeader
WHERE 1=1
AND TotalDue > @Threshold
AND YEAR(ORDERDATE) >= @StartYear
AND YEAR (ORDERDATE) <= @EndYear

END --Ending stored procedure
GO -- wrap stored procedure in "go" if you need to include query in the same block

EXEC dbo.OrdersAboveThreshold 20000, 2010, 2012 -- Executing stored procedure, with multiple parameters, comma delimit with no parens
DROP PROCEDURE dbo.OrdersAboveThreshold
----------------------------------------------------------------
----------------------------------------------------------------
/*
Create a stored procedure that allows users to search for employees by name.  Allow the user to select 'exact match', 'begins with', 'ends with', or 'contains'
using integers 1-4.

*/

USE AdventureWorks2019
GO
CREATE PROCEDURE dbo.NameSearch(@NameToSearch VARCHAR(32), @SearchString VARCHAR(32), @MatchType INT)
AS
BEGIN
DECLARE @NameSearchExecutible VARCHAR(MAX)

IF @MatchType = 1
	BEGIN
		SET @NameSearchExecutible = 'SELECT
		*
		FROM
		Person.Person
		WHERE 1=1
		AND '

		SET @NameSearchExecutible = @NameSearchExecutible + @NameToSearch

		SET @NameSearchExecutible = @NameSearchExecutible + '='

		SET @NameSearchExecutible = @NameSearchExecutible + @SearchString
	END
IF @MatchType = 2
	BEGIN
		SET @NameSearchExecutible = 'SELECT
		*
		FROM
		Person.Person
		WHERE 1=1
		AND '

		SET @NameSearchExecutible = @NameSearchExecutible + @NameToSearch + 'Name'

		SET @NameSearchExecutible = @NameSearchExecutible + ' LIKE '

		SET @NameSearchExecutible = @NameSearchExecutible + '''' + @SearchString + '%' + ''''
	END
IF @MatchType = 3
	BEGIN
		SET @NameSearchExecutible = 'SELECT
		*
		FROM
		Person.Person
		WHERE 1=1
		AND '

		SET @NameSearchExecutible = @NameSearchExecutible + @NameToSearch + 'Name'

		SET @NameSearchExecutible = @NameSearchExecutible + ' LIKE '

		SET @NameSearchExecutible = @NameSearchExecutible + '''' + '%' + @SearchString  + ''''
	END
IF @MatchType = 4
	BEGIN
		SET @NameSearchExecutible = 'SELECT
		*
		FROM
		Person.Person
		WHERE 1=1
		AND '

		SET @NameSearchExecutible = @NameSearchExecutible + @NameToSearch + 'Name'

		SET @NameSearchExecutible = @NameSearchExecutible + ' LIKE '

		SET @NameSearchExecutible = @NameSearchExecutible + '''' + '%' + @SearchString + '%' + ''''
	END

EXEC (@NameSearchExecutible)
END
GO
EXEC dbo.NameSearch 'Last', 'ille', 4
DROP PROCEDURE dbo.NameSearch







