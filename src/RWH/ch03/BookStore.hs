data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

type CustomerID = Int
type ReviewBody = String

data BookReview = BookReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                   | CashOnDelivery
                   | Invoice CustomerID
                     deriving (Show)

myInfo = Book 9283984 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

-- Using Record Syntax
data Customer = Customer {
      customerID :: CustomerID,
      customerName :: String,
      customerAddress :: Address
} deriving (Show)

customer1 = Customer 212818 "Gabriel Rios" ["Rua X", "Rua Y"]

-- can vary the order of fields
customer2 = Customer {
              customerID = 1093138,
              customerName = "Paul Hudak",
              customerAddress = ["Rua X", "Rua Y"]
}