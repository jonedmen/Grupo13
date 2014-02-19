class CreateItems < ActiveRecord::Migration
  def change
    create_table :ventas do |t|
      t.string :localidad

      t.timestamps
    end
		create_table :items do |t|
			t.belongs_to :venta      
			t.string :nombre
      t.string :valor

      t.timestamps
    end
  end
end
