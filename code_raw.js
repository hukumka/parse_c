var block = function (){var block = new Block();
block.add(new Action('int y = 0', function(namespace){namespace.define('y', 0);}));
block.add(new Condition('x<0', function(namespace){return namespace.get('x')<0}, function (){var block = new Block();
block.add(new Action('x = -x', function(namespace){namespace.set('x', -namespace.get('x'));}));
return block;}(), undefined));
block.add(new ForLoop('for(int i = 0; i<x; i += 1){', function(namespace){namespace.define('i', 0);}, function(namespace){return namespace.get('i')<namespace.get('x')}, function(namespace){namespace.set('i', namespace.get('i')+1);}, function (){var block = new Block();
block.add(new Action('y += i', function(namespace){namespace.set('y', namespace.get('y')+(namespace.get('i')));}));
return block;}()));
block.add(new Return('return 2*y+x', function(namespace){return 2*namespace.get('y')+namespace.get('x');}));
return block;}()
