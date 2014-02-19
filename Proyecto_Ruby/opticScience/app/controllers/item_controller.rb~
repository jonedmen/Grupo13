class ItemController < ApplicationController
def index
    @clients = Client.all
		@items = Item.all
  end

  def new
		@item = Item.new
  end

  def create
		@item = Item.new(params.require(:item).permit(:nombre,:valor))
    if @item.save
      redirect_to client_index_path, :notice => "Se registro correctamente"
    else
      render :action => "new"
    end
  end
  
  def destroy
		@item = Item.find(params[:id])
    @item.destroy
    redirect_to client_index_path, :notice => "Se ah eliminado correctamente"
  end

  def edit
		@item = Item.find(params[:id])
  end

  def update
		@item = Item.find(params[:id])
    if @item.update_attributes(params.require(:item).permit(:nombre,:valor))
      redirect_to client_index_path, :notice => "Se actualizo correctamente"
    else
      render "edit"
    end
  end
end
