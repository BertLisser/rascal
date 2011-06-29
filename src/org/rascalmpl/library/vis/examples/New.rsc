module vis::examples::New

import vis::Figure;
import vis::Render;
import Real;
import Integer;
import List;


public void overlay1(){
	render(overlay([
		box(fillColor("red")),
		box(fillColor("green"),shrink(0.6)),
		box(fillColor("orange"),shrink(0.3))
	],shrink(0.9)));
}

public void overlay2(){
	render(overlay([
		box(fillColor("red")),
		box(fillColor("green"),shrink(0.6),left()),
		box(fillColor("orange"),shrink(0.3),right(),bottom())
	],shrink(0.9)));
}

public void shape1(){
	render(overlay([
		ellipse(shrink(0.05),fillColor("red"),align(x,y)) 
		| <x,y> <- [<0.0,0.0>,<0.0,1.0>,<1.0,1.0>,<1.0,0.0>]],shapeConnected(true),shapeClosed(true),fillColor("orange")
	));
}

	

public void path(int n){
	render(overlay([
		ellipse(shrink(0.02),fillColor("blue"),align((1.0/toReal(n)) * toReal(x) ,arbReal()))
		| x <- [0..n]],shapeConnected(true),shapeCurved(true),shapeClosed(true),fillColor("red")));
}

public void graphgrow(int n){
	render(
		
			bottomAxis("x",
			leftAxis("y",
				overlay([
					ellipse(shrink(0.02),fillColor("blue"),hpos(convert((1.0/toReal(n)) * toReal(x) , "x")),vpos(convert(arbReal(),"y")))
					| x <- [0..n]],shapeConnected(true),shapeCurved(true))
			,vgrow(1.2)	
		),shrink(0.7),hgrow(1.2))
	);
}



public void nominalKeyTest(){
	render(
		hcat(
				[box(fillColor(convert(s,"type"))) | 
				s <- ["Rascal","C++","Java"]] + [nk("Types","type")])
				);
}



public void star(int n){
	piInc = PI() / toReal(n);
	angle = 0.0;
	
	coord = for(i <- [1..(2*n)]){
		radius = (i % 2 == 0) ? 0.5 : 0.2;
		append <sin(angle) * radius + 0.5 ,cos(angle) * radius + 0.5>;
		angle += piInc;
	}
	
	render(overlay([
		ellipse(shrink(0.02),fillColor("red"),align(x,y)) 
		| <x,y> <- coord],shapeConnected(true),shapeClosed(true),fillColor("orange")
	));
}

public void bubbles(int n){
	render(overlay([
		ellipse(
			hshrink(arbReal() * 0.4 + 0.1),
			vshrink(arbReal() * 0.4 + 0.1),
			fillColor(rrgba(arbReal(),arbReal(),arbReal(),arbReal())),
			align(arbReal(),arbReal())
		)
		| i <- [1..n]]));
}

public void mondriaan(){
	// Painting by Piet Mondriaan: Composition II in Red, Blue, and Yellow, 1930
	render(grid([
			[
				vcat([box(),box()],hshrink(0.2),vshrink(0.8))
				,box(fillColor("red"))
			],
	 		[
	 			box(fillColor("blue")),
	 			hcat([
	 				  box(hshrink(0.9)),
	 				  vcat([box(),box(fillColor("yellow"))])
					 ])
	 		]
		],stdLineWidth(6)));
} 


public void dutchFlag(){
	render(vcat([box(fillColor("red")),box(),box(fillColor("blue"))]));
}

public void frenchFlag(){
	render(hcat([box(fillColor("red")),box(),box(fillColor("blue"))]));
}

public void vennDiagram(){
	render(overlay([
		ellipse(text("A"),left(),top(),shrink(0.6),fillColor(color("red",0.6))),
		ellipse(text("B"),right(),top(),shrink(0.6),fillColor(color("green",0.6))),
		ellipse(text("C"),bottom(),shrink(0.6),fillColor(color("blue",0.6)))
		]));
}