using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ActualDelete : QuickButton
{
    private List<Transform> toDelete = new List<Transform>();
    public Scroll scroll;

    // Use this for initialization
    protected override void Start()
    {
        base.Start();
    }

    public void addToDelete(Transform d)
    {
        toDelete.Add(d);
    }

    protected override void ButtonEnterBehavior(GameObject other)
    {
        scroll.DeleteObjects(toDelete);
        toDelete.Clear();
    }

    protected override void ButtonExitBehavior(GameObject other) { }
    // Update is called once per frame
    void Update()
    {

    }
}
