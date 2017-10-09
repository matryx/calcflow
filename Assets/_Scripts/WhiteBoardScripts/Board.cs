using UnityEngine;
using System.Collections;
using System.Collections.Generic;

/* Class name: Board
 * Description: handles all functionality related to the Board
 */
public class Board : MonoBehaviour {
	private List<GameObject> letters, label;
    private Stack<GameObject> undo, redo;
    //private Stack<GameObject> freeUndo, freeRedo;
	private ButtonManager buttonManager;
    private Transform frame;
    private static Vector3 minScale = new Vector3(2.5f, 1f, 0.5f);
    //private static Vector3 screenShotPos = new Vector3(0, 0, -1f);
    private static Vector3 collapsedDelSize = new Vector3(0.1f, 0.2f, 0.1f);
    private static Vector3 collapsedDelPos = new Vector3(-.442f, -.336f, -.507f);
    private static Vector3 expandedDelSize = new Vector3(0.045f, .09f, 1f);
    private static Vector3 expandedDelPos = new Vector3(-.4575f, -.397f, -.528f);
    private static float minDistToBoard = 2.5f;
    //private static float scale = 10f;
    private float currx, curry, currz;
    private bool animating = false;
    private bool collapsed = false;

	public static List<Board> boards = new List<Board>();	
	public GameObject mirror;
	public Camera boardCam;
    public float speed = 3f;

    internal static Vector3 boardSize = new Vector3(200f, 100f, 10f);

    public void Awake() {
		buttonManager = ButtonManager.createButtonManager (this);

		initialize ();
		buttonManager.manageButtons ();
		//boards.Add (this);
	}

	private void initialize() {
		mirror = MirrorBoard.createMirror (gameObject);
		boardCam = mirror.GetComponent<MirrorBoard> ().getCam ();
		frame = transform.Find ("Frame");
		letters = new List<GameObject>();
		label = new List<GameObject>();
		undo = new Stack<GameObject> ();
		redo = new Stack<GameObject> ();
    }

	private void FixedUpdate(){
	}

	// Description: calls the function that takes a screenshot
	public void print (){
		//boardCam.gameObject.GetComponent<HiResScreenShots> ().TakeHiResShot ();
	}

	// Description: creates a board object and adds it to list of existing boards 
	public static Board createBoard() {
		Board temp = (Instantiate(Resources.Load("Board", typeof(GameObject))) as GameObject).AddComponent<Board>();
		temp.fastCollapse ();
        boards.Add(temp);
		return temp;
	}

	/* Description: goes through all existing boards and determines which one
	 *				is closest to the passed in point pos
	 * Return Value: board closest to the point
	 */	
	public static Board getClosestBoard(Vector3 pos) {
		Board closest = null;
		float minDist = 0;
		float currDist = 0;

		if (boards == null) return null;

		foreach (Board b in boards) {
			currDist = (pos - b.GetComponent<Collider>().ClosestPointOnBounds(pos)).magnitude;

			if (!closest) {
				minDist = currDist;
				closest = b;
			}

			if (currDist < minDist) {
				minDist = currDist;
				closest = b;
			}
		}

        if (minDist > minDistToBoard) return null;

		return closest;
	}

	// Description: collapses board
	public IEnumerator Collapse() {
		float tParam = 0;

		if (animating) yield return null;

		animating = true;
        hideLetters();
		buttonManager.disableButtons ();

		while(transform.localScale != minScale) {
			if (tParam < 1) {
				tParam += Time.deltaTime * speed; //This will increment tParam based on Time.deltaTime multiplied by a speed multiplier
				transform.localScale = new Vector3(Mathf.Lerp(boardSize.x, minScale.x, tParam),
												   Mathf.Lerp(boardSize.y, minScale.y, tParam),
												   Mathf.Lerp(boardSize.z, minScale.z, tParam));
			}
			yield return null;
		}
			
		collapsed = true;
		animating = false;
		buttonManager.manageButtons();
	}

	// Description: makes the board collapse into label without animation
	private void fastCollapse(){
		transform.localScale = minScale;
		collapsed = true;
		buttonManager.manageButtons();
		hideLetters ();
	}

	// Description: expands board
	public IEnumerator Expand(){
		float tParam = 0;

		if (animating) yield return null;

		animating = true;
		buttonManager.disableButtons ();
		yield return null;

		showLetters();

		while (transform.localScale != boardSize) {
			if (tParam < 1) {
				tParam += Time.deltaTime * speed; //This will increment tParam based on Time.deltaTime multiplied by a speed multiplier
				transform.localScale = new Vector3(Mathf.Lerp(minScale.x, boardSize.x, tParam),
												   Mathf.Lerp(minScale.y, boardSize.y, tParam),
												   Mathf.Lerp(minScale.z, boardSize.z, tParam));
			}
			yield return null;
		}

		collapsed = false;
		animating = false;
        showLetters();
        buttonManager.manageButtons();
    }

    // Description: called to disable buttons when board is snapped into a slot
    public void disableButtons ()
	{
		buttonManager.disableButtons ();
	}

	// Description: called to enable buttons when board is taken out of a slot
	public void enableButtons() 
	{
		buttonManager.manageButtons ();

	}

	// Description: returns boolean indicating whether or not the board is collapsed
	public bool isCollapsed() {
		return collapsed;
	}

	// Description: returns boolean indicating whether or not board is animating 
	public bool isAnimating() {
		return animating;
	}

	// Description: rotates board by 90 degrees
	public void snapBoard() {
		transform.Rotate (new Vector3(0f, 90f, 0f));
	}

	// Description: deletes current board
	public void deleteBoard() {
		boards.Remove (this);
		Destroy (this.gameObject);
	}

	// Description: adds Line parameter into the correct list of lines
	public void addLetter(Line letter) {
		if (collapsed) {
			label.Add (letter.gameObject);
		} else if (!collapsed) {
            letters.Add(letter.gameObject);
        } /*else if (fDraw) {
			freeLetters.Add (letter.gameObject);
		} */
	}
		
	// Description: adds passed in letter to stack of undo letters
	public void addUndoLetter(GameObject let) {
        undo.Push(let);

        /*
        if (free) {
            freeUndo.Push(let);
        } else {
            undo.Push(let);
        }*/
    }

	// Description: undoes drawing or erase
	public void undoAction() {
		GameObject pop = null;

		if (undo.Count != 0) pop = undo.Pop ();

		if (!pop) return;
		
		if (pop.activeSelf) {
			pop.SetActive (false);
		} else {
			letters.Add (pop);
			pop.SetActive (true);
		}

		redo.Push (pop);
	}

	// Description: redoes the undo action
	public void redoAction() {
		GameObject pop = null;

		if (redo.Count != 0) pop = redo.Pop ();

		if (!pop) return;

		if (pop.activeSelf) {
			pop.SetActive (false);
		} else {
			letters.Add (pop);
			pop.SetActive (true);
		}
	}

    // Description: clears redo list
    public void clearRedo() {
        redo.Clear();

        /*
        if (free) {
            freeRedo.Clear();
        } else {
            redo.Clear();
        }*/
    }

    /* Description: shows lines drawn on boards and 3D space, hides 
	 * 				lines drawn on collapsed board
	 */
    public void showLetters(){
		foreach(GameObject letter in letters) letter.SetActive (true);	

		//foreach(GameObject letter in freeLetters) letter.SetActive (true);	

		foreach(GameObject letter in label) letter.SetActive (false);
	}

	/* Description: hides lines drawn on boards and 3D space, shows
	 * 				lines drawn on collapsed board
	 */
	public void hideLetters(){
		foreach(GameObject letter in letters) letter.SetActive (false);

		//foreach(GameObject letter in freeLetters) letter.SetActive (false);

		foreach(GameObject letter in label) letter.SetActive (true);
	}


	// Description: returns the correct List of letters
	public List<GameObject> getLetters () {
		if (isCollapsed()) return label;

		return letters;

        /*else if (fDraw) {
			return freeLetters;
		}*/
    }

    // Description: highlights the frame of an active board
    public void highlight(){
		foreach (Transform piece in frame) {
			piece.gameObject.GetComponent<Renderer> ().material.SetColor ("_Color", Color.blue);
		}
	}

	// Description: unhighlights the frame of an inactive board
	public void unhighlight(){
		foreach (Transform piece in frame) {
			piece.gameObject.GetComponent<Renderer> ().material.SetColor ("_Color", Color.grey);
		}
	}

	void OnDestroy() {
        boards.Remove(this);
		Destroy (mirror);
	} 

/*-------------------------------------------- Mirror Board-------------------------------------------------------*/

	internal class MirrorBoard : MonoBehaviour {
		private static int numBoards = 0;

		private GameObject board;
		private Camera camerac;

		internal static GameObject createMirror(GameObject board){
			GameObject cube = Instantiate (Resources.Load ("Painter", typeof(GameObject))) as GameObject;
			MirrorBoard mirror = cube.AddComponent<MirrorBoard> ();
			mirror.board = board;
			return mirror.gameObject;
		}

		private int resWidth = 2000; 
		private int resHeight = 2000;

		void Awake(){
			camerac = transform.Find ("Camera").gameObject.GetComponent<Camera>();		

		}

		void Start(){
			transform.position += new Vector3 (numBoards++*2, 50, 0);
			camerac = transform.Find ("Camera").gameObject.GetComponent<Camera>();		
			RenderTexture rt = new RenderTexture(resWidth, resHeight, 24);
			camerac.targetTexture = rt;
			board.transform.Find ("highpoly_board").GetComponent<Renderer> ().material.mainTexture = rt;
			//board.GetComponent<Renderer>().
		}
		
		internal Camera getCam(){
			return camerac;
		}

	}

	/*-------------------------------------------- Button Manager-------------------------------------------------------*/

	internal class ButtonManager : MonoBehaviour{
		internal Transform min, max, screenshot, delete, rotate, undo, redo;
		internal List<Transform> buttons = new List<Transform>();
		private Board board;

		internal static ButtonManager createButtonManager(Board board){
			ButtonManager buttonManager = board.gameObject.AddComponent<ButtonManager> ();
			buttonManager.board = board;
			return buttonManager;
		}

		void Awake() {
			board = GetComponent<Board> ();
			initButtons ();
		}



		internal void disableButtons() {
			foreach (Transform button in buttons) {
				button.gameObject.SetActive (false);
			}
		}

		internal void manageButtons() {
			manageDelete ();
			manageMinMax ();
			managescreenShot ();
			manageRotate ();
			manageUndo ();
			manageRedo ();
		}

		// Description: manages active state of screenshot button
		internal void managescreenShot() {
			if (board.isCollapsed ()) {
				screenshot.gameObject.SetActive (false);
			} else {
				screenshot.gameObject.SetActive (true);
			}
		}

		// Description: manages active state of rotate button
		internal void manageRotate () {
			if (board.isCollapsed ()) {
				rotate.gameObject.SetActive (false);
			} else {
				rotate.gameObject.SetActive (true);
			}
		}

		// Description: manages active state of undo button
		internal void manageUndo() {
			if (board.isCollapsed ()) {
				undo.gameObject.SetActive (false);
			} else {
				undo.gameObject.SetActive (true);
			}
		}

		internal void manageRedo() {
			if (board.isCollapsed ()) {
				redo.gameObject.SetActive (false);
			} else {
				redo.gameObject.SetActive (true);
			} 
		}

		// Description: manage the active state of minimize and maximize button
		internal void manageMinMax() {
			if (board.isAnimating()) {
				min.gameObject.SetActive (false);
				max.gameObject.SetActive (false);
			} else if (board.isCollapsed ()) {
				max.gameObject.SetActive (true);
                min.gameObject.SetActive (false);
			} else {
				min.gameObject.SetActive (true);
				max.gameObject.SetActive (false);
			}
		}

		// Description: manages position and scale of delete button
		internal void manageDelete() {
			delete.gameObject.SetActive (true);
			if (board.isCollapsed ()) {
				delete.localScale = collapsedDelSize;
				delete.localPosition = collapsedDelPos;
			} else {
                delete.localScale = expandedDelSize;
                delete.localPosition = expandedDelPos;
			}
		}

		private void initButtons(){
			delete = board.transform.Find ("Delete");
			buttons.Add (delete);
			min = board.transform.Find ("Minimize");
			buttons.Add (min);
			max = board.transform.Find ("Maximize");
			buttons.Add (max);
			screenshot = board.transform.Find ("ScreenShot");
			buttons.Add (screenshot);
			redo = board.transform.Find ("Redo");
			buttons.Add (redo);
			undo = board.transform.Find ("Undo");
			buttons.Add (undo);
			rotate = board.transform.Find ("Rotate");
			buttons.Add (rotate);
		}
	}
}
