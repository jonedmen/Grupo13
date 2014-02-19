class CreateVenta < ActiveRecord::Migration
  def change
    create_table :venta do |t|
      t.string :localidad

      t.timestamps
    end

		create_table :clients do |t|
			t.belongs_to :venta
			t.string :cedula
			t.timestamps
		end
  end
end
