using UnityEngine;
using System.Collections;

public class helper : MonoBehaviour {
    public GameObject mobius;
    //int w = 0;

    // Use this for initialization
    void Start() { 
        
    }

    // Update is called once per frame
    void Update () {
        Mobius mob = mobius.GetComponent<Mobius>();
        mob.dragPoint(transform.position);
    }
}

