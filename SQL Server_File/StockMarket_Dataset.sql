create database StockMarket_Dataset

use 
StockMarket_Dataset
go

select * from cf							--Main data
select * from Example_Returns				--Weekly Returns data

--Agrregate Operations:

select min(Price) from cf					--minimum price of Stock
select max(price) from cf					--maximum price of Stock

select min(EBITDA) from cf					-- minimum of EBITDA
select max(EBITDA) from cf					--maximum of EBITDA

select min([Market Cap]) from cf			-- minimum of market cap
select max([market cap]) from cf			-- maximum of market cap 

select min([52 week low]) from cf			--minimum of 52 week low	
select max([52 Week Low]) from cf			--maximum of 52 week low

select min([52 week high]) from cf			--minimum of 52 week high
select max([52 week high]) from cf			--maximum of 52 week high

-- Grouping the Names sector wise 

select Sector,count(*) as No_of_Companies
from cf
group by Sector

-- Grouping.
select sector,MAX([Earnings/share]) as MAX_EPS
from cf
group by Sector

-- Checking the NULL values in the dataset.
select * from cf
where[Price] is null or [Price/Earnings] is null or [Dividend Yield] is null or
[Earnings/Share] is null or [52 Week Low] is null or [52 Week High] is null or 
[Market Cap] is null or [EBITDA] is null or [Price/Sales] is null or [Price/Book]  is null


