import System.IO
import System.IO.Unsafe
import Data.List.Split
import Data.List

--spliterInicial : Funcion para separar los devices de la cabezera principal
spliterInicial :: [Char]->[[Char]]
spliterInicial xml = endBy "</device>\n" temp2
		    where temp2 = head(splitOn "</devices>\n" temp)
			  temp = last(splitOn "<devices>\n" xml)

--getNames : Funcion que separa cada nombre y modelo de device en una lista
getNames :: [[Char]]->[([Char],[Char])]
getNames [] = []
getNames (x:xs) = names:getNames xs
		where   names = (brand_nameF,model_nameF)
			model_nameF = case (length(model_name)>10) of  
          				True -> (concat(tail(splitOn "model_name" model_name)))
          				False ->(concat(tail(splitOn "model_name" model_name)))++"Generic" 
			brand_nameF = case (length(brand_name)>10) of  
          				True -> (concat(tail(splitOn "brand_name" brand_name)))
          				False ->(concat(tail(splitOn "brand_name" brand_name)))++"Generic" 
			model_name = (concat(filter (isInfixOf "model_name") zy))
			brand_name = (concat(filter (isInfixOf "brand_name") zy))
			zy = linesBy (=='\n') u
			u = concat(splitOneOf " " x) 

--searchDevicesforGeneric : Funcion que obtiene devices con modelo y manufacturador
searchDevicesforGeneric :: [[Char]]->[[Char]]
searchDevicesforGeneric devices = filter (isInfixOf "model_name") devices

--cleanContent : Funcion que limpia el contenido del xml como lo son los caracteres especiales
cleanContent :: [[Char]]->[[Char]]
cleanContent [] = []
cleanContent (x:xs) = zxy:cleanContent xs
		    where zxy = "\n     Device Information:\n"++concat(tail(splitOn "product_info" zx))
			  zx = concat(splitOn "device" z)
		       	  z = concat(splitOn " id" y)
		       	  y = concat(splitOn "group" w)
		       	  w = concat(splitOn "value" v)
		       	  v = concat(splitOn "capability name" u)
		       	  u = concat(splitOneOf "</>\"=" x) 

--groupDevicebyManufacturer : Funcion que busca los devices por manufacturador y los bota en una lista
groupDevicebyManufacturer :: [Char]->[([Char],[Char])]->[[Char]]
groupDevicebyManufacturer manufacturer [] = []
groupDevicebyManufacturer manufacturer ((x,y):xy) = splitOneOf "" (concat(model:groupDevicebyManufacturer manufacturer xy))
							where model = case (manufacturer==x) of
								      True -> y++"\n"
								      False -> ""


--groupManufacturer : Funcion que bota los Manufacturadores ordenados y sin repeticion
groupManufacturer :: [([Char],[Char])]->[[Char]]
groupManufacturer [] = []
groupManufacturer ((x,y):xy) = nub(sort(x:groupManufacturer xy))


--deviceInformation : Funcion bota en un string la informacion del dispositivo
deviceInformation :: [Char]->[(([Char],[Char]),[Char])]->[Char]
deviceInformation device [] = error "Select a correct device\n"
deviceInformation device (((x,y),z):xyz) = deviceF
					   where deviceF = case (device == y) of
							   True -> z
							   False -> deviceInformation device xyz

--groupDevicebyResolution : Funcion que valida devices con resoluciones anchoxalto
groupDevicebyResolution :: [(([Char],[Char]),[Char])]->[[Char]]
groupDevicebyResolution [] = []
groupDevicebyResolution (((x,y),z):xyz) = devices
					 where devices= (x++"\n"++y++"\n"++(concat(splitOneOf "_" z))):groupDevicebyResolution xyz 

--groupDevicewithResolution : Funcion que junta cada device con su respectiva resolucion widthxheight
groupDevicewithResolution :: [[Char]]->[([Char],[Char],[Char])]
groupDevicewithResolution [] = []
groupDevicewithResolution (x:xs) = devices:groupDevicewithResolution xs
				  where devices = (device_manufacturer,device_name,width++"x"++height)
					width = concat(splitOneOf " " (concat(splitOn "resolutionwidth" (concat(filter (isInfixOf "resolutionwidth") a)))))
					height = concat(splitOneOf " " (concat(splitOn "resolutionheight" (concat(filter (isInfixOf "resolutionheight") a)))))
					device_name = head(tail(a))
					device_manufacturer= head(a)
					a = linesBy (=='\n') (head(list2))
					list2 = filter (isInfixOf "resolutionwidth") list
					list = filter (isInfixOf "resolutionheight") (x:xs)

--manufacturerwithResolution : Funcion que imprime los manufacturadores con resolucion habilitada
manufacturerwithResolution :: [([Char],[Char],[Char])] -> [[Char]]
manufacturerwithResolution [] = []
manufacturerwithResolution ((x,y,z):xyz) = sort(nub(x:manufacturerwithResolution xyz))

--devicewithResolution : Funcion que imprime las resoluciones por device
devicewithResolution :: [Char]->[([Char],[Char],[Char])]->[[Char]]
devicewithResolution manufacturer [] = []
devicewithResolution manufacturer ((x,y,z):xyz) = splitOneOf "" (concat(resolutions:devicewithResolution manufacturer xyz))
						where resolutions = case (manufacturer == x) of
								   True -> z++"\n"
								   False -> ""
					
--deviceByResolution : Funcion que bota en una lista los dispositivos con la resolucion requerida
deviceByResolution :: [Char]->[Char]->[([Char],[Char],[Char])]->[[Char]]
deviceByResolution manufacturer resolution [] = []
deviceByResolution manufacturer resolution ((x,y,z):xyz) = splitOneOf "" (concat(model:deviceByResolution manufacturer resolution xyz))
					where model = case ((manufacturer++resolution) == (x++z)) of
						     True -> y++"\n"
						     False -> ""
--searchbyQuery : Funcion que busca dispositivos por los queries proveidos y los bota en una lista
searchbyQuery :: [Char]->[Char]->[Char]->[[Char]]->[[Char]]
searchbyQuery query1 query2 query3 [] = []
searchbyQuery query1 query2 query3 (x:xs) = device
						where device = filter (isInfixOf query1) temp
						      temp = filter (isInfixOf query2) temp2
						      temp2 = filter (isInfixOf query3) (x:xs)

--printDevicesbyQuery : Funcion que imprime los dispositivos que cumplen con los queries
printDevicesbyQuery :: [[Char]]->[[Char]]
printDevicesbyQuery [] = []
printDevicesbyQuery (x:xs) = a:printDevicesbyQuery xs
				where a = head(tail(splitOn "model_name " b))
				      b = head(filter (isInfixOf "model_name") c)
				      c = linesBy (=='\n') x

main = do
	handle<-openFile "wurfl-2.3.xml" ReadMode
	let contents=unsafeDupablePerformIO(hGetContents handle)
	let devices = cleanContent (searchDevicesforGeneric (spliterInicial contents))
        let deviceListbyName = zip (getNames devices) devices 
	functions devices deviceListbyName handle
	hClose handle
functions devices deviceListbyName handle = do
	putStrLn "\n8888b.  888888 Yb    dP 88  dP\"\"b8 888888"
	putStrLn " 8I  Yb 88__    Yb  dP  88 dP   `\" 88__  "
	putStrLn " 8I  dY 88\"\"     YbdP   88 Yb      88\"\"  "
	putStrLn "8888Y\"  888888    YP    88  YboodP 888888"
	putStrLn "\n88\"\"Yb 88\"\"Yb  dP\"Yb  Yb        dP .dP\"Y8 888888 88\"\"Yb"
	putStrLn "88__dP 88__dP dP   Yb  Yb  db  dP  `Ybo.\" 88__   88__dP"
	putStrLn "88\"\"Yb 88\"Yb  Yb   dP   YbdPYbdP   o.`Y8b 88\"\"   88\"Yb "
	putStrLn "88oodP 88  Yb  YbodP     YP  YP    8bodP' 888888 88  Yb\n\n"
	putStr "Menu:\n\t1.Search by Manufacturer\n\t2.Search by Resolution\n\t3.Search by Queries\n\t4.Developer Information\n\t5.Exit Program\n"	
	putStrLn "\nSelect Option:"
	
	option <- getLine
	
	if option == "1" then do print(groupManufacturer (getNames devices))
			         putStrLn "\nPick the Manufacturer to browse:"
				 manufacturer <-getLine
				 putStrLn "\nManufacturer devices:"
				 let manufacturers = sort (endBy "\n" (concat(groupDevicebyManufacturer manufacturer (getNames devices))))
				 if (null(manufacturers)) then do putStrLn ("\nNot a valid manufacturer\n")
								  temp <- getLine
								  functions devices deviceListbyName handle
				 else do print (manufacturers)
				         putStrLn "\nWhich device do you wish to browse information?:"
				         device <-getLine
				         let devicesbyManufacturer = deviceInformation device deviceListbyName
				         if (null(devices)) then do putStrLn ("Not a valid device")
						                    temp <- getLine
							            functions devices deviceListbyName handle
				         else do putStrLn(devicesbyManufacturer)
				                 putStrLn "\n\nTHANKS FOR USING DEVICE BROWSER\n"
				                 option <- getLine
				                 functions devices deviceListbyName handle
	else if option == "2" then do let deviceResolution = groupDevicewithResolution (groupDevicebyResolution deviceListbyName)
				      print (manufacturerwithResolution deviceResolution)
				      putStrLn "\nPick the Manufacturer to browse:"
				      manufacturer <-getLine
				      let manufacturerList = nub(sort (endBy "\n" (concat(devicewithResolution manufacturer (deviceResolution)))))
				      if(null(manufacturerList)) then do putStrLn ("\nNot a valid manufacturer\n")
								         temp <- getLine
								         functions devices deviceListbyName handle
				      else do putStrLn ("\nAvailable Resolutions for "++manufacturer++":")
					      print(manufacturerList)
				              putStrLn "\nNow put the desired resolution as widthxheight:"
				              resolution <-getLine
				              let answer = deviceByResolution manufacturer resolution (deviceResolution)
				              if (null(answer)) then do putStrLn ("\nNot a valid resolution\n")
								        temp <- getLine
								        functions devices deviceListbyName handle
					      else do putStrLn ("\nManufacturer: "++manufacturer)
					      	      putStrLn "\nDevices with the selected resolution:"
					      	      print (nub(sort (endBy "\n" (concat(answer)))))
				                      putStrLn "\n\nTHANKS FOR USING DEVICE BROWSER\n"
				      		      option <- getLine
				                      functions devices deviceListbyName handle
				      
	else if option == "3" then do putStrLn "\nEnter the queries you wish to browse:"
				      putStrLn "\nEnter first query:"	
				      query1<-getLine
				      putStrLn "Enter second query:"
			              query2<-getLine
				      putStrLn "Enter third query:"
				      query3<-getLine
				      putStrLn "\nProcessing..."
				      putStrLn ("\nNumber of devices found:"++show(length(searchbyQuery query1 query2 query3 devices)))
				      print (nub(sort(printDevicesbyQuery(searchbyQuery query1 query2 query3 devices))))
				      putStrLn "\n\nTHANKS FOR USING DEVICE BROWSER\n"
				      temp <- getLine
				      functions devices deviceListbyName handle
				      
	else if option == "4" then do putStrLn "\nDeveloped in HASKELL by Jonathan Mendieta\n\tStudent in ESPOL\n\t\tProgramming Lenguage Project\n"
				      option <- getLine
				      functions devices deviceListbyName handle
	else if option =="5" then do hClose handle 
				     return()
	else do putStrLn "\nERROR: Select a valid option"
		option <- getLine
	        functions devices deviceListbyName handle
