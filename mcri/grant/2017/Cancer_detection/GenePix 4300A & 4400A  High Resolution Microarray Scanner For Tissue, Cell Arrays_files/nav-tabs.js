// this script picks apart the url and if a var names tabs is set it forces that tab to select using the tabbers functions
$(document).ready(function(){
						   
						   $("#contraint_div").fadeTo(1200,1);
						 
						   var hu = window.location.search.substring(1);
						   var qs = hu.split("&");
						
								for(var a = 0; a < qs.length ; a++){
									var temp = qs[a].split("=");
									if(temp[0] == "tabs"){
										tabbers(temp[1]);	
									}
									
								}
						 
});