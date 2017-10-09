using UnityEngine;
using System.Collections;

public class BoardContainer : MonoBehaviour {
	//add to current slot's x value
	private Vector3 slotOffset, bbOffset, boardScale;
	private Transform slots, bbox;

	// Use this for initialization
	public void Start () {
		slots = transform.Find ("Slots");
		slotOffset = new Vector3 (2.9f, 0, 0);
		bbox = transform.Find ("BlackBox");
		bbOffset = new Vector3 (slotOffset.x, 0, 0);
        boardScale = new Vector3(2.5f, 1f, .5f);
		spawnBoards ();
	}

	public void Update(){
		foreach (Board board in Board.boards) {
			snapIntoPlace (board);
		}
	}

	// Description: spawns the first four boards on awake
	private void spawnBoards() {
		foreach(Transform child in slots) {
			Board board = Board.createBoard ();
            board.transform.position = Vector3.zero;
            board.transform.rotation = Quaternion.identity;

            board.transform.SetParent(child.transform, false);
            board.transform.localScale = boardScale;

        }
    }
		
	// Description: adds a board and a slot if more slots needed, otherwise just adds a board to an existing empty slot
	public void addBoards() {
		if (slots.childCount == Board.boards.Count) {
			GameObject newSlot = Instantiate(Resources.Load("SlotPrefab", typeof(GameObject))) as GameObject;
            newSlot.transform.SetParent(slots, false);
            newSlot.name = "Slot (" + (slots.childCount) + ")";
           
            if (slots.childCount % 2 == 0) {
				newSlot.transform.localPosition = slots.Find("Slot (" + (slots.childCount-2) + ")").localPosition + slotOffset;

				bbox.localScale += bbOffset;
				bbox.localPosition += bbOffset/2;
			} else {
				newSlot.transform.localPosition = slots.Find("Slot (" + (slots.childCount-2) + ")").localPosition + slotOffset;
			}
           
            Board b = Board.createBoard ();
            b.transform.SetParent (newSlot.transform, false);
            b.transform.localScale = boardScale;
        }
        else {
			//add new board to existing empty slot
			foreach(Transform child in slots) {
				if (!child.Find("Board(Clone)")) {
					Board b = Board.createBoard ();
                    b.transform.SetParent(child.transform, false);
                    b.transform.localScale = boardScale;

                    return;
				}
			}
		}
	}

	// Description: handles the drifting of a collapsed board into a slot
	private void snapIntoPlace(Board board) {
		slots.gameObject.layer = 0;
		Vector3 boardPos = board.transform.position;
		float minDist = 0;
		float currDist = 0;
		Transform closest = null;

        if (board.GetComponent<GrabbableObject>().IsGrabbed == true) return;

		foreach(Transform child in slots) {
			currDist = (boardPos - child.transform.position).magnitude;

			if (!closest) {
				minDist = currDist;
				closest = child;
			}

			if (currDist < minDist) {
				minDist = currDist;
				closest = child;
			}
		}

		if ((!board.isCollapsed()) || minDist > 3f) 
		{
			board.enableButtons ();
		} else if (!closest.Find("Board(Clone)")) {
            
			//if (board.transform.eulerAngles != closest.transform.eulerAngles) {
			//	board.transform.eulerAngles = closest.transform.eulerAngles;
			//}

			board.gameObject.transform.SetParent(closest, true);
            
            if (board.transform.localScale != boardScale) {
                board.transform.localScale = boardScale;
            }
		} else if (closest == board.transform.parent) {
            if (Mathf.Abs((board.transform.position - closest.transform.position).magnitude) < .0001)
            {
                board.transform.position = closest.transform.position;
            }
            else {
                board.transform.position = Vector3.Lerp(board.transform.position, closest.transform.position, .08f);
            }
            if (board.transform.localRotation != Quaternion.identity)
            {
                board.transform.localRotation = Quaternion.RotateTowards(board.transform.localRotation, Quaternion.identity, 1f);
            }

            board.disableButtons ();
		}
	}
}
