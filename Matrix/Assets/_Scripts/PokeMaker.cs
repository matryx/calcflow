using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class PokeMaker : MonoBehaviour
{

    string nameOfLTip = "hand_left_renderPart_0/hands:l_hand_world/hands:b_l_hand/hands:b_l_index1/hands:b_l_index2/hands:b_l_index3/hands:b_l_index_ignore";
    string nameOfRTip = "hand_right_renderPart_0/hands:r_hand_world/hands:b_r_hand/hands:b_r_index1/hands:b_r_index2/hands:b_r_index3/hands:b_r_index_ignore";

    // Use this for initialization
    void Start()
    {
        StartCoroutine(FindTheDamnFingerTip());
    }


    IEnumerator FindTheDamnFingerTip()
    {
        while (true)
        {
            Transform foundL = transform.Find(nameOfLTip);
            Transform foundR = transform.Find(nameOfRTip);
            if (foundL != null)
            {
                addCollider(foundL.gameObject);
                break;
            }
            else if (foundR != null)
            {
                addCollider(foundR.gameObject);
                break;
            }
            else yield return null;
        }
    }

    void addCollider(GameObject gameObject)
    {
        gameObject.layer = LayerMask.NameToLayer("ButtonPresser");
        SphereCollider col = gameObject.AddComponent<SphereCollider>();
        col.radius = .01f;
        col.center = new Vector3 (-.01f,0,0);
        col.isTrigger = true;
        gameObject.AddComponent<Rigidbody>().isKinematic = true;
    }
}
