using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using NanoVRController;
//using OvrTouch.Hands;

public class FreeMarker : ControlScheme {
    //public Hand anim;
    private Graph graph;
    private Line currFreeLine;
    private GameObject freeEraser;
    private Material lineMat;
    private Vector3 curr;
    private Vector3 prev;
    //private Vector3 offset = new Vector3(-1f, 0, 0);
    private float eraserRadius = .25f;
    private float freePenThickness = .05f;
    private float letterMinDraw = .0001f;
    

    private bool firstFreeLine = true;
    private bool freeDrawing = false;
    private bool freeErasing;

    public VRController controller;

    private ControllerComponent dummyArgs;

    public override void SetController(VRController c)
    {
        controller = c;
    }

    public override void Enable()
    {
        ConnectController();
        //if (anim)
        //{
        //    anim.setMode(Hand.MODE.marker);
        //}
    }

    public void ConnectController()
    {
        controller.components[ButtonId.GRIP].ComponentPressed += startFreeErasing;
        controller.components[ButtonId.GRIP].ComponentUnpressed += stopFreeErasing;
        controller.components[ButtonId.TRIGGER].ComponentPressed += startFreeDrawing;
        controller.components[ButtonId.TRIGGER].ComponentUnpressed += stopFreeDrawing;
    }

    public override void Disable()
    {
        DisconnectController();
    }

    public void DisconnectController()
    {
        stopFreeDrawing();
        stopFreeErasing();
        controller.components[ButtonId.GRIP].ComponentPressed -= startFreeErasing;
        controller.components[ButtonId.GRIP].ComponentUnpressed -= stopFreeErasing;
        controller.components[ButtonId.TRIGGER].ComponentPressed -= startFreeDrawing;
        controller.components[ButtonId.TRIGGER].ComponentUnpressed -= stopFreeDrawing;
        //if (anim)
        //{
        //    anim.setMode(Hand.MODE.hand);
        //}
    }

    public void Awake()
    {
        initialize();
    }

    private void FixedUpdate()
    {
        setGraph();

        if (!graph) return;

        manageFreeEraser();
        freeDraw();
        freeErase();
    }

    public static FreeMarker addFreeMarker(GameObject newFreeMarker)
    {
        return newFreeMarker.AddComponent<FreeMarker>();
    }

    // Description: initializes instance variables, called on awake
    private void initialize()
    {
        lineMat = (Material)Resources.Load("WhiteBoard/LineMat", typeof(Material));
        freeEraser = Instantiate(Resources.Load("WhiteBoard/FreeEraser", typeof(GameObject))) as GameObject;
        freeEraser.transform.localScale = new Vector3(eraserRadius, eraserRadius, eraserRadius);
        freeEraser.SetActive(false);
    }

    public void setGraph()
    {
        Graph nextGraph = Graph.getClosestGraph(transform.position);

        if (graph != nextGraph)
        {
            stopFreeDrawing ();
            stopFreeErasing ();
            if (graph) graph.unhighlight();
        }

        if (nextGraph)
        {
            nextGraph.highlight();
            graph = nextGraph;
        }
    }

    public Graph getGraph()
    {
        return graph;
    }

    // Description:moves the eraser with the pen and disables and enables the eraser

    private void manageFreeEraser()
    { 

        if (!graph)
        {
            freeEraser.SetActive(false);
            return;
        }
        
        freeEraser.transform.position = transform.position;

		if (freeErasing) {
			freeEraser.SetActive (true);
		} else {
			freeEraser.SetActive (false);
		}
    }



    /*
     * --------------------------------------------------
     * ----------------FREEDRAW FUNCTIONS----------------
     * --------------------------------------------------
     */

    // Description: allows marker to start free drawing in 3D space
    public void startFreeDrawing(VRController c, ControllerComponentArgs e)
    {
        startFreeDrawing();
    }

    public void startFreeDrawing()
    {
        freeDrawing = true;
    }

    // Description: handles free drawing in 3D space, needs to be called constantly
    private void freeDraw()
    {
        if (freeDrawing)
        {
            graph.clearRedo();
            curr = transform.position;

            if (!firstFreeLine)
            {
                if (Vector3.Magnitude(prev - curr) > letterMinDraw)
                {
                    drawFreeLine(curr);
                    prev = curr;
                }
            }
            else
            {
                drawFreeLine(curr);
                prev = curr;
                firstFreeLine = false;
            }
        }
    }



    // Description: calls functions from Line to create lines for free draw function
    private void drawFreeLine(Vector3 p)
    {
        if (firstFreeLine)
        {
            currFreeLine = Line.createLine();
            currFreeLine.setWidth(freePenThickness);

            currFreeLine.transform.SetParent(graph.transform, true);

            currFreeLine.setMaterial(lineMat);
            currFreeLine.startLine(p);
        }
        else
        {
            currFreeLine.addPoint(p);
        }
    }

    // Description: stops marker from free drawing in 3D space
    public void stopFreeDrawing(VRController c, ControllerComponentArgs e)
    {
        stopFreeDrawing();
    }

    // Description: stops marker from free drawing in 3D space
    public void stopFreeDrawing()
    {
        firstFreeLine = true;

        if (!currFreeLine)
        {
            freeDrawing = false;
            return;
        }

        graph.addLetter(currFreeLine);
        graph.addUndoLetter(currFreeLine.gameObject);
        freeDrawing = false;
        currFreeLine = null;
    }

    public bool getfreeDrawing()
    {
        return freeDrawing;
    }

    /*
     * ----------------------------------------------------
     * ----------------FREEERASER FUNCTIONS----------------
     * ----------------------------------------------------
     */

    // Description: allows marker to start erasing lines drawn on 3D space
    public void startFreeErasing(VRController c, ControllerComponentArgs e)
    {
        startFreeErasing();
    }

    // Description: allows marker to start erasing lines drawn on 3D space
    public void startFreeErasing()
    {
        freeErasing = true;
    }

    // Description: calls Board function to erase lines drawn on 3D space
    public void freeErase()
    {
        GameObject letterDelete;

        if (freeErasing)
        {
            graph.clearRedo();
            letterDelete = touchingLetters(transform.position);

            if (!letterDelete) return;

            graph.addUndoLetter(letterDelete);
            letterDelete.SetActive(false);
        }
    }

    // Description: allows marker to start erasing lines drawn on 3D space
    public void stopFreeErasing(VRController c, ControllerComponentArgs e)
    {
        stopFreeErasing();
    }

    // Description: stops marker from erasing lines drawn on 3D space
    public void stopFreeErasing()
    {
        freeErasing = false;
    }

    private GameObject touchingLetters(Vector3 pos)
    {
        List<GameObject> letterList = graph.getLetters();

        foreach (GameObject letter in letterList)
        {
            for (int i = 0; i < letter.GetComponent<Line>().getNumPoints() - 1; i++)
            {
                if ((pos - letter.GetComponent<Line>().getPoint(i)).magnitude < eraserRadius)
                {
                    letterList.Remove(letter);
                    return letter;
                }
            }
        }
        return null;
    }
}
