class Venta < ActiveRecord::Base
	has_one :item
	has_one :client
end
