function showVariable(name, symIds)
{

  var ids = symIds[name];
//  alert("showing symbols " + name + " " + ids.length);
  for(var i =0; i < ids.length; i++) {
     var el = document.getElementById(ids[i]);
     el.className = "showSymbol";
  }

}

function hideVariable(name, symIds)
{

  var ids = symIds[name];
  for(var i =0; i < ids.length; i++) {
     var el = document.getElementById(ids[i]);
     el.className = "symbol";
  }

}
