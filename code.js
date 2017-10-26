var block = function (){var block = new Block();
block.add(new Action('int y = x+3;', function(namespace){namespace.define('y', namespace.get('x')+3);}));
block.add(new Condition('y>2', function(namespace){return namespace.get('y')>2}, function (){var block = new Block();
block.add(new Action('y = 3;', function(namespace){namespace.set('y', 3);}));
block.add(new Action('y -= x;', function(namespace){namespace.set('y', namespace.get('y')-(namespace.get('x')));}));
return block;}(), new Condition('y==2', function(namespace){return namespace.get('y')==2}, function (){var block = new Block();
block.add(new Action('y *= 3;', function(namespace){namespace.set('y', namespace.get('y')*(3));}));
return block;}(), function (){var block = new Block();
block.add(new Action('y = 4;', function(namespace){namespace.set('y', 4);}));
return block;}())));
block.add(new Action('y += 4;', function(namespace){namespace.set('y', namespace.get('y')+(4));}));
return block;}()
