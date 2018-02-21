using UnityEngine;
using System.Collections;
using System.Collections.Generic;
using VoxelBusters.RuntimeSerialization;

[RuntimeSerializable(typeof(MonoBehaviour), true, true)]
public class Graph : MonoBehaviour {

    //public static Graph instance = null;

    public static List<Graph> graphs = new List<Graph>();
    private List<GameObject> freeLetters;
    private Stack<GameObject> freeUndo, freeRedo;

    //private static float minDistToGraph = 2.5f;

    public void Awake()
    {
        initialize();
        graphs.Add(this);
    }

    private void initialize()
    {
        freeUndo = new Stack<GameObject>();
        freeRedo = new Stack<GameObject>();
        freeLetters = new List<GameObject>();
    }

    public static Graph getClosestGraph(Vector3 pos)
    {
        Graph closest = null;
        float minDist = 0;
        float currDist = 0;

        if (graphs == null) return null;

        foreach (Graph g in graphs)
        {
            currDist = (pos - g.transform.position).magnitude;

            if (!closest)
            {
                minDist = currDist;
                closest = g;
            }

            if (currDist < minDist)
            {
                minDist = currDist;
                closest = g;
            }
        }

        //if (minDist > minDistToGraph) return null;
   
        return closest;
    }

    public void undoFreeAction()
    {
        GameObject pop = null;

        if (freeUndo.Count != 0) pop = freeUndo.Pop();

        if (!pop) return;

        if (pop.activeSelf)
        {
            pop.SetActive(false);
        }
        else
        {
            freeLetters.Add(pop);
            pop.SetActive(true);
        }

        freeRedo.Push(pop);
    }

    public void redoFreeAction()
    {
        GameObject pop = null;

        if (freeRedo.Count != 0) pop = freeRedo.Pop();

        if (!pop) return;

        if (pop.activeSelf)
        {
            pop.SetActive(false);
        }
        else
        {
            freeLetters.Add(pop);
            pop.SetActive(true);
        }
    }

    // Description: clears redo list
    public void clearRedo()
    {
        freeRedo.Clear();
    }

    public List<GameObject> getLetters()
    {
        return freeLetters;
    }

    public void addLetter(Line letter)
    {
        freeLetters.Add(letter.gameObject); 
    }

    // Description: adds passed in letter to stack of undo letters
    public void addUndoLetter(GameObject let)
    {
        freeUndo.Push(let);   
    }

    // Description: highlights the frame of an active board
    public void highlight()
    {
        //gameObject.transform.Find("Wireframe").GetComponent<Renderer>().material.SetColor("_Color", Color.green);
        
    }

    // Description: unhighlights the frame of an inactive board
    public void unhighlight()
    {
        //gameObject.GetComponent<Renderer>().material.SetColor("_Color", Color.grey);
    }

    void OnDestroy()
    {
        graphs.Remove(this);

    }
}
