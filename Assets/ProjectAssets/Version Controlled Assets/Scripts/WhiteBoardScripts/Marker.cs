using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using System;

/* Class name: Marker
 * Description: Handles all marker functionality (drawing on board and 3D space, erasing, crosshairs)
 */
public class Marker : MonoBehaviour
{
    private Board board;
    private Line currLine;
    private GameObject crosshair;
    private GameObject eraser;
    private Vector3 curr;
    private Vector3 prev;

    private float offset = -0.05f;
    private float eraserRadius = .02f;
    private float penThickness = .0005f;
    private float crosshairRadius = .2f;
    private float scale = 10f;
    private bool firstLine = true;
    private bool drawing;
    private bool erasing;

    //minimum thickness of lines
    private float letterMinDraw = .0001f;
    private float labelMinDraw = .002f;

    public void Awake()
    {
        scaleMarker();
        initialize();
    }

    internal void startFreeDrawing()
    {
        throw new NotImplementedException();
    }

    // Description: initializes instance variables, called on awake
    private void initialize()
    {
        crosshair = Instantiate(Resources.Load("CrossHairs", typeof(GameObject))) as GameObject;
        crosshair.SetActive(false);
        eraser = Instantiate(Resources.Load("Eraser", typeof(GameObject))) as GameObject;
        eraser.SetActive(false);
    }

    internal void stopFreeDrawing()
    {
        throw new NotImplementedException();
    }

    //Description: calls functions that need to be updated constantly
    private void FixedUpdate()
    {
        setBoard();

        if (!board) return;

        manageCrossHairs();
        erase();
        draw();
    }

    // Description: fix the scaling of marker's components 
    private void scaleMarker()
    {
        crosshairRadius *= scale;
        penThickness *= scale;
        eraserRadius *= scale;
        letterMinDraw *= scale;
        labelMinDraw *= scale;
    }

    /* Description: adds Marker class component to a controller's base attach point,
	 * 				allowing access to all pubic functions in this class
	 * Return Value: Marker attached to game object 
	 */
    public static Marker addMarker(GameObject newMarker)
    {
        return newMarker.AddComponent<Marker>();
    }

    // Description: activates and de-activates Marker based on passed in boolean
    public void setMarkerActive(bool activate)
    {
        if (activate)
        {
            if (board) board.highlight();
            enabled = true;
            if (crosshair && board)
            {
                crosshair.SetActive(true);
                eraser.SetActive(true);
            }
        }
        else {
            if (board) board.unhighlight();
            enabled = false;
            stopDrawing();
            stopErasing();
            if (crosshair)
            {
                crosshair.SetActive(false);
                eraser.SetActive(false);
            }
        }
    }

    /* Description: calls a Board function to find out which board the marker
	 * 				can interact with
	 */
    public void setBoard()
    {
        Board nextBoard = Board.getClosestBoard(transform.position);

        if (board != nextBoard)
        {
            stopDrawing();
            stopErasing();
            if (board) board.unhighlight();
        }

        if (nextBoard)
        {
            nextBoard.highlight();
            board = nextBoard;
        }
    }

    // Description: returns the current board
    public Board getBoard()
    {
        return board;
    }

    // Description: determines if Marker is hitting the board 
    public bool hittingBoard(out RaycastHit hit)
    {
        if (!(penTarget(out hit)) || hit.transform.name != "highpoly_board")
        {
            return false;
        }

        return true;
    }

    // Description: determines if ray is hitting the board
    public bool penTarget(out RaycastHit hit)
    {
        return Physics.Raycast(transform.position, board.transform.rotation * Vector3.forward, out hit, 100.0f);
    }

    // Description: manages activation and de-activation of crosshair, eraser and free eraser objects
    private void manageCrossHairs()
    {
        RaycastHit hit;
        Vector3 boardLoc;
        crosshair.transform.localScale = new Vector3(crosshairRadius, crosshairRadius, 0);
        eraser.transform.localScale = new Vector3(eraserRadius, eraserRadius, 0);

        if (!board)
        {
            crosshair.SetActive(false);
            eraser.SetActive(false);
            return;
        }

        crosshair.transform.rotation = board.transform.rotation;
        eraser.transform.rotation = board.transform.rotation;

        if (board && hittingBoard(out hit))
        {
            boardLoc = hit.point + board.transform.rotation * Vector3.forward * offset;

            if (erasing)
            {
                eraser.SetActive(true);
                eraser.transform.position = boardLoc;
                crosshair.SetActive(false);
            }
            else {
                crosshair.SetActive(true);
                crosshair.transform.position = boardLoc;
                eraser.SetActive(false);
            }
        }
        else {
            crosshair.SetActive(false);
            eraser.SetActive(false);
        }
    }

    // Description: returns line that is being touched by passed in pos and deletes it from list
    private GameObject touchingLetters(Vector3 pos)
    {
        List<GameObject> letterList = board.getLetters();
        float radius = eraserRadius / 20;

        foreach (GameObject letter in letterList)
        {
            for (int i = 0; i < letter.GetComponent<Line>().getNumPoints() - 1; i++)
            {
                if ((pos - letter.GetComponent<Line>().getPoint(i)).magnitude < radius)
                {
                    letterList.Remove(letter);
                    return letter;
                }
            }
        }
        return null;
    }

    /*
     * ----------------------------------------------
     * -----------------DRAW FUNCTIONS---------------
     * ----------------------------------------------
     */
    // Description: allows marker to start drawing on the board
    public void startDrawing()
    {
        drawing = true;
    }

    // Description: handles drawing on the board, needs to be called constantly
    private void draw()
    {
        RaycastHit hit;
        float currMinDraw;

        if (!board) return;

        if (!hittingBoard(out hit) || board.isAnimating())
        {
            stopDrawing();
            return;
        }

        if (drawing)
        {
            board.clearRedo();
            curr = mapToMirror(hit);

            if (!firstLine)
            {
                if (board.isCollapsed())
                {
                    currMinDraw = labelMinDraw;
                }
                else {
                    currMinDraw = letterMinDraw;
                }

                if (Vector3.Magnitude(prev - curr) > currMinDraw)
                {
                    drawLine(curr);
                    prev = curr;
                }
            }
            else {
                drawLine(curr);
                prev = curr;
                firstLine = false;
            }
        }
    }

    // Description: calls functions from Line class to creates lines for draw function 
    private void drawLine(Vector3 p)
    {
        if (firstLine)
        {
            currLine = Line.createLine();
            currLine.setWidth(penThickness);

            currLine.transform.SetParent(board.mirror.transform, false);
            currLine.gameObject.layer = LayerMask.NameToLayer("Painter");
            Material lineMat = (Material)Resources.Load("LineMat", typeof(Material));
            currLine.setMaterial(lineMat);
            currLine.startLine(p + currLine.transform.parent.transform.position);
        }
        else {
            currLine.addPoint(p + currLine.transform.parent.transform.position);
        }
    }

    // Description: stops marker from drawing on board
    public void stopDrawing()
    {
        drawing = false;
        firstLine = true;

        if (!currLine) return;

        board.addLetter(currLine);
        board.addUndoLetter(currLine.gameObject);
        currLine = null;
    }

    /*
     * ------------------------------------------------ 
     * ----------------ERASER FUNCTIONS----------------
     * ------------------------------------------------
     */
    // Description: allows marker to start erasing lines drawn on the board
    public void startErasing()
    {
        erasing = true;
    }


    // Description: calls Board function to erase lines on the board
    public void erase()
    {
        RaycastHit hit;
        GameObject letterDelete;

        if (!board) return;

        if (!hittingBoard(out hit) || board.isAnimating()) stopErasing();

        if (erasing)
        {
            board.clearRedo();
            letterDelete = touchingLetters(mapToMirror(hit) + board.mirror.transform.position);

            if (!letterDelete) return;

            board.addUndoLetter(letterDelete);
            letterDelete.SetActive(false);
        }
    }

    // Description: stops marker from erasing on the board
    public void stopErasing()
    {
        erasing = false;
    }

    private Vector3 mapToMirror(RaycastHit point)
    {
        Vector3 map;
        map.x = point.textureCoord.x - board.boardCam.orthographicSize;
        map.y = point.textureCoord.y - board.boardCam.orthographicSize;
        map.z = 0f;
        return map;
    }

}
