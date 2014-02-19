class CreateClients < ActiveRecord::Migration
  def change
    create_table :clients do |t|
      t.string :cedula
      t.string :nombre
      t.string :apellido

      t.timestamps
    end
  end
end
