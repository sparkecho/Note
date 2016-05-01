"作为示范: self 和 super 的用法 line:65 ~ line:77"



"watches.st"
"A program which defines classes about watches to demonstrate the usage of the super keyword."

Object subclass: Watch [
    | style chronometerCapability |
    
    <comment: 'A class for defining ordinary watches.'>
    
    setStyle: theStyle [
        "A method to set the style of the watch."
	    
        style := theStyle.
    ]
    
    getStyle [
        "A method to get the style of the watch."
	    
        ^style
    ]
    
    setChronometerCapability: theChronometerCapability [
        "A method to specify if the watch has chronometer capabilities."
	    
        chronometerCapability := theChronometerCapability.
    ]
    
    getChronometerCapability [
        "A method to determine if the watch has chronometer capabilities."
	    
        ^chronometerCapability.
    ]
    
    listYourFeatures [
        "A method to print out the features of the watch."
	    
        Transcript show: 'Style: ', self getStyle; cr.
        Transcript show: 'Chronometer capabilities: ', self getChronometerCapability printString; cr.
    ]
]

Watch subclass: WaterResistantWatch [
    | resistanceDepth |
    
    <comment: 'A class for defining water resistant watches.'>
    
    setResistanceDepth: aDepth [
        "A method to set the resistance depth of the watch."
	    
        resistanceDepth := aDepth.
    ]
    
    getResistanceDepth [
        "A method to get the resistance depth of the watch."
	    
        ^resistanceDepth.
    ]
    
    listYourFeatures [
        "A method to print out the features of the water resistant watch."

        "self listYourFeatures."
        <comment: '这样做的后果是会溢出, 因为这样写是对自己的调用, 称为无限递归.'>

        "self superclass: listYourFeatures."
        <comment: '这样做的后果是提前退出, 因为调用的是基类的此函数的完整实现, 会直接返回, 下面的 Transcript 语句将不会执行'>


        <comment: '这样做才是正解, 调用基类的该函数而不返回, 能够正确实现这项功能.'>
        <comment: '这样的注释不能放在实际执行的代码中间, 必须放在定义开头, 否则会报 parse error, expected \] 这样的错误'>
        super listYourFeatures.

	    "comment"
        "而这样的注释则可以在执行的代码间放置"

        Transcript show: 'Resistance depth: ', self getResistanceDepth printString; cr.
    ]
]

| watch1 watch2 |

watch1 := Watch new.
watch1 setStyle: 'Analog'.
watch1 setChronometerCapability: true.

Transcript show: 'The features of the 1st watch:'; cr.
Transcript show: '------------------------------'; cr.
watch1 listYourFeatures.


Transcript cr.


watch2 := WaterResistantWatch new.
watch2 setStyle: 'Digital'.
watch2 setResistanceDepth: 30.

Transcript show: 'The features of the 2nd watch:'; cr.
Transcript show: '------------------------------'; cr.
watch2 listYourFeatures.
