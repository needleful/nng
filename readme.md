# needleful net generator
This is a rewrite of my [old site generator, npsg](https://github.com/needleful/npsg).
I use C# for work, so I didn't want to use it everywhere. Plus, the code was getting quite verbose.
Prolog has some fancy symbolic manipulation that's theoretically well-suited for the task.

## Writing templates
The examples in `test/` show a lot of the system's capabilities.

You make a template file:

```XML
<template name='hello'>
	<param name='person'/>
	<content>
		<link rel='stylesheet' href='/needleful.css'/>
		<!--Content is filled in with double brackets-->
		<o>Hello, <i>[[person]]!</i></o>
	</content>
</template>
``` 

You make a page file:

```XML
<hello>
	<person>Gamer</person>
</hello>
```

And it gets turned into HTML:
```HTML
<!DOCTYPE html>
<link rel="stylesheet" href="/needleful.css"><o>Hello, <i>Gamer!</i></o>
```