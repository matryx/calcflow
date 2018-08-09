using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public interface ExpressionTabInterface
{
    ExpressionSet GetExpSet();
    ExpressionActions GetExpActions();
    Transform GetExpressionX();
    Transform GetSeparator();

    void SetButtonInputColor(Color col);
    void SetElementQuadTex(Texture tex);
    void SetTextColor(Color c);
    void SetExpressionX(Transform e);
    void SetSeparator(Transform sep);
    void DeleteExpressionFromScroll();
    bool GetActiveStatus();
}
